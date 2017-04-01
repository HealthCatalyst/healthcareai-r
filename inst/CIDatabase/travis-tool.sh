#!/bin/bash
# -*- sh-basic-offset: 4; sh-indentation: 4 -*-
# Bootstrap an R/travis environment.

set -e
# Comment out this line for quieter output:
set -x

CRAN=${CRAN:-"https://cran.rstudio.com"}
BIOC=${BIOC:-"http://bioconductor.org/biocLite.R"}
BIOC_USE_DEVEL=${BIOC_USE_DEVEL:-"TRUE"}
OS=$(uname -s)

PANDOC_VERSION='1.13.1'
PANDOC_DIR="${HOME}/opt/pandoc"
PANDOC_URL="https://s3.amazonaws.com/rstudio-buildtools/pandoc-${PANDOC_VERSION}.zip"

# MacTeX installs in a new $PATH entry, and there's no way to force
# the *parent* shell to source it from here. So we just manually add
# all the entries to a location we already know to be on $PATH.
#
# TODO(craigcitro): Remove this once we can add `/usr/texbin` to the
# root path.
PATH="${PATH}:/usr/texbin"

R_BUILD_ARGS=${R_BUILD_ARGS-"--no-manual"}
R_CHECK_ARGS=${R_CHECK_ARGS-"--no-manual --as-cran"}

R_USE_BIOC_CMDS="source('${BIOC}');"\
" tryCatch(useDevel(${BIOC_USE_DEVEL}),"\
" error=function(e) {if (!grepl('already in use', e$message)) {e}});"\
" options(repos=biocinstallRepos());"

Bootstrap() {
    if [[ "Darwin" == "${OS}" ]]; then
        BootstrapMac
    elif [[ "Linux" == "${OS}" ]]; then
        BootstrapLinux
    else
        echo "Unknown OS: ${OS}"
        exit 1
    fi

    if ! (test -e .Rbuildignore && grep -q 'travis-tool' .Rbuildignore); then
        echo '^travis-tool\.sh$' >>.Rbuildignore
    fi
}

InstallPandoc() {
    local os_path="$1"
    mkdir -p "${PANDOC_DIR}"
    curl -o /tmp/pandoc-${PANDOC_VERSION}.zip ${PANDOC_URL}
    unzip -j /tmp/pandoc-${PANDOC_VERSION}.zip "pandoc-${PANDOC_VERSION}/${os_path}/pandoc" -d "${PANDOC_DIR}"
    chmod +x "${PANDOC_DIR}/pandoc"
    sudo ln -s "${PANDOC_DIR}/pandoc" /usr/local/bin
    unzip -j /tmp/pandoc-${PANDOC_VERSION}.zip "pandoc-${PANDOC_VERSION}/${os_path}/pandoc-citeproc" -d "${PANDOC_DIR}"
    chmod +x "${PANDOC_DIR}/pandoc-citeproc"
    sudo ln -s "${PANDOC_DIR}/pandoc-citeproc" /usr/local/bin
}

BootstrapLinux() {
    # Set up our CRAN mirror.
    sudo add-apt-repository "deb ${CRAN}/bin/linux/ubuntu $(lsb_release -cs)/"
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

    # Add marutter's c2d4u repository.
    sudo add-apt-repository -y "ppa:marutter/rrutter"
    sudo add-apt-repository -y "ppa:marutter/c2d4u"

    # Update after adding all repositories.  Retry several times to work around
    # flaky connection to Launchpad PPAs.
    Retry sudo apt-get update -qq

    # Install an R development environment. qpdf is also needed for
    # --as-cran checks:
    #   https://stat.ethz.ch/pipermail/r-help//2012-September/335676.html
    Retry sudo apt-get install -y --no-install-recommends r-base-dev r-recommended qpdf

    # Change permissions for /usr/local/lib/R/site-library
    # This should really be via 'staff adduser travis staff'
    # but that may affect only the next shell
    sudo chmod 2777 /usr/local/lib/R /usr/local/lib/R/site-library

    # Process options
    BootstrapLinuxOptions
}

BootstrapLinuxOptions() {
    if [[ -n "$BOOTSTRAP_LATEX" ]]; then
        # We add a backports PPA for more recent TeX packages.
        sudo add-apt-repository -y "ppa:texlive-backports/ppa"

        Retry sudo apt-get install -y --no-install-recommends \
            texlive-base texlive-latex-base texlive-generic-recommended \
            texlive-fonts-recommended texlive-fonts-extra \
            texlive-extra-utils texlive-latex-recommended texlive-latex-extra \
            texinfo lmodern
    fi
    if [[ -n "$BOOTSTRAP_PANDOC" ]]; then
        InstallPandoc 'linux/debian/x86_64'
    fi
}

BootstrapMac() {
    # Install from latest CRAN binary build for OS X
    wget ${CRAN}/bin/macosx/R-latest.pkg  -O /tmp/R-latest.pkg

    echo "Installing OS X binary package for R"
    sudo installer -pkg "/tmp/R-latest.pkg" -target /
    rm "/tmp/R-latest.pkg"

    # Process options
    BootstrapMacOptions
}

BootstrapMacOptions() {
    if [[ -n "$BOOTSTRAP_LATEX" ]]; then
        # TODO: Install MacTeX.pkg once there's enough disk space
        MACTEX=BasicTeX.pkg
        wget http://ctan.math.utah.edu/ctan/tex-archive/systems/mac/mactex/$MACTEX -O "/tmp/$MACTEX"

        echo "Installing OS X binary package for MacTeX"
        sudo installer -pkg "/tmp/$MACTEX" -target /
        rm "/tmp/$MACTEX"
        # We need a few more packages than the basic package provides; this
        # post saved me so much pain:
        #   https://stat.ethz.ch/pipermail/r-sig-mac/2010-May/007399.html
        sudo tlmgr update --self
        sudo tlmgr install inconsolata upquote courier courier-scaled helvetic
    fi
    if [[ -n "$BOOTSTRAP_PANDOC" ]]; then
        InstallPandoc 'mac'
    fi
}

EnsureDevtools() {
    if ! Rscript -e 'if (!("devtools" %in% rownames(installed.packages()))) q(status=1)' ; then
        # Install devtools and testthat.
        RBinaryInstall devtools testthat
        Rscript -e "install.packages('https://github.com/krlmlr/devtools/raw/develop/bin/devtools_1.12.0.9001.zip', repos = NULL)"
    fi
}

AptGetInstall() {
    if [[ "Linux" != "${OS}" ]]; then
        echo "Wrong OS: ${OS}"
        exit 1
    fi

    if [[ "" == "$*" ]]; then
        echo "No arguments to aptget_install"
        exit 1
    fi

    echo "Installing apt package(s) $@"
    Retry sudo apt-get -y install "$@"
}

DpkgCurlInstall() {
    if [[ "Linux" != "${OS}" ]]; then
        echo "Wrong OS: ${OS}"
        exit 1
    fi

    if [[ "" == "$*" ]]; then
        echo "No arguments to dpkgcurl_install"
        exit 1
    fi

    echo "Installing remote package(s) $@"
    for rf in "$@"; do
        curl -OL ${rf}
        f=$(basename ${rf})
        sudo dpkg -i ${f}
        rm -v ${f}
    done
}

RInstall() {
    if [[ "" == "$*" ]]; then
        echo "No arguments to r_install"
        exit 1
    fi

    echo "Installing R package(s): $@"
    Rscript -e 'install.packages(commandArgs(TRUE), repos="'"${CRAN}"'", INSTALL_opts="--no-multiarch")' "$@"
}

BiocInstall() {
    if [[ "" == "$*" ]]; then
        echo "No arguments to bioc_install"
        exit 1
    fi

    echo "Installing R Bioconductor package(s): $@"
    Rscript -e "${R_USE_BIOC_CMDS}"' biocLite(commandArgs(TRUE))' "$@"
}

RBinaryInstall() {
    if [[ -z "$#" ]]; then
        echo "No arguments to r_binary_install"
        exit 1
    fi

    if [[ "Linux" != "${OS}" ]] || [[ -n "${FORCE_SOURCE_INSTALL}" ]]; then
        echo "Fallback: Installing from source"
        RInstall "$@"
        return
    fi

    echo "Installing *binary* R packages: $*"
    r_packages=$(echo $* | tr '[:upper:]' '[:lower:]')
    r_debs=$(for r_package in ${r_packages}; do echo -n "r-cran-${r_package} "; done)

    AptGetInstall ${r_debs}
}

InstallGithub() {
    EnsureDevtools

    echo "Installing GitHub packages: $@"
    # Install the package.
    Rscript -e 'library(devtools); library(methods); options(repos=c(CRAN="'"${CRAN}"'")); install_github(commandArgs(TRUE), build_vignettes = FALSE)' "$@"
}

InstallDeps() {
    EnsureDevtools
    Rscript -e 'library(devtools); library(methods); options(repos=c(CRAN="'"${CRAN}"'")); install_deps(dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))'
}

InstallBiocDeps() {
    EnsureDevtools
    Rscript -e "${R_USE_BIOC_CMDS}"' library(devtools); install_deps(dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))'
}

DumpSysinfo() {
    echo "Dumping system information."
    R -e '.libPaths(); sessionInfo(); installed.packages()'
}

DumpLogsByExtension() {
    if [[ -z "$1" ]]; then
        echo "dump_logs_by_extension requires exactly one argument, got: $@"
        exit 1
    fi
    extension=$1
    shift
    package=$(find . -maxdepth 1 -name "*.Rcheck" -type d)
    if [[ ${#package[@]} -ne 1 ]]; then
        echo "Could not find package Rcheck directory, skipping log dump."
        exit 0
    fi
    for name in $(find "${package}" -type f -name "*${extension}"); do
        echo ">>> Filename: ${name} <<<"
        cat ${name}
    done
}

DumpLogs() {
    echo "Dumping test execution logs."
    DumpLogsByExtension "out"
    DumpLogsByExtension "log"
    DumpLogsByExtension "fail"
}

RunTests() {
    echo "Building with: R CMD build ${R_BUILD_ARGS}"
    if [[ "${OS:0:5}" == "MINGW" ]]; then
        if [[ -d vignettes ]]; then
            rm -rf vignettes
            Rscript -e "d <- read.dcf('DESCRIPTION'); d[, colnames(d) == 'VignetteBuilder'] <- NA; write.dcf(d, 'DESCRIPTION')"
        fi
    fi

    # We want to grab the version we just built.
    FILE=$(ls -1t *.tar.gz | head -n 1)

    # Create binary package (currently Windows only)
    if [[ "${OS:0:5}" == "MINGW" ]]; then
        R_CHECK_INSTALL_ARGS="--install-args=--build --no-multiarch"
    fi

    echo "Testing with: R CMD check \"${FILE}\" ${R_CHECK_ARGS} ${R_CHECK_INSTALL_ARGS}"
    _R_CHECK_CRAN_INCOMING_=${_R_CHECK_CRAN_INCOMING_:-FALSE}
    if [[ "$_R_CHECK_CRAN_INCOMING_" == "FALSE" ]]; then
        echo "(CRAN incoming checks are off)"
    fi
    _R_CHECK_CRAN_INCOMING_=${_R_CHECK_CRAN_INCOMING_} R_QPDF=true R CMD check "${FILE}" ${R_CHECK_ARGS} ${R_CHECK_INSTALL_ARGS}

    # Check examples flagged with \donttest
    devtools::run_examples()
    
    # Check reverse dependencies
    if [[ -n "$R_CHECK_REVDEP" ]]; then
        echo "Checking reverse dependencies"
        Rscript -e 'library(devtools); checkOutput <- unlist(revdep_check(as.package(".")$package));if (!is.null(checkOutput)) {print(data.frame(pkg = names(checkOutput), error = checkOutput));for(i in seq_along(checkOutput)){;cat("\n", names(checkOutput)[i], " Check Output:\n  ", paste(readLines(regmatches(checkOutput[i], regexec("/.*\\.out", checkOutput[i]))[[1]]), collapse = "\n  ", sep = ""), "\n", sep = "")};q(status = 1, save = "no")}'
    fi

    if [[ -n "${WARNINGS_ARE_ERRORS}" ]]; then
        if DumpLogsByExtension "00check.log" | grep -q WARNING; then
            echo "Found warnings, treated as errors."
            echo "Clear or unset the WARNINGS_ARE_ERRORS environment variable to ignore warnings."
            exit 1
        fi
    fi
}

Retry() {
    if "$@"; then
        return 0
    fi
    for wait_time in 5 20 30 60; do
        echo "Command failed, retrying in ${wait_time} ..."
        sleep ${wait_time}
        if "$@"; then
            return 0
        fi
    done
    echo "Failed all retries!"
    exit 1
}

COMMAND=$1
echo "Running command: ${COMMAND}"
shift
case $COMMAND in
    ##
    ## Bootstrap a new core system
    "bootstrap")
        Bootstrap
        ;;
    ##
    ## Ensure devtools is loaded (implicitly called)
    "install_devtools"|"devtools_install")
        EnsureDevtools
        ;;
    ##
    ## Install a binary deb package via apt-get
    "install_aptget"|"aptget_install")
        AptGetInstall "$@"
        ;;
    ##
    ## Install a binary deb package via a curl call and local dpkg -i
    "install_dpkgcurl"|"dpkgcurl_install")
        DpkgCurlInstall "$@"
        ;;
    ##
    ## Install an R dependency from CRAN
    "install_r"|"r_install")
        RInstall "$@"
        ;;
    ##
    ## Install an R dependency from Bioconductor
    "install_bioc"|"bioc_install")
        BiocInstall "$@"
        ;;
    ##
    ## Install an R dependency as a binary (via c2d4u PPA)
    "install_r_binary"|"r_binary_install")
        RBinaryInstall "$@"
        ;;
    ##
    ## Install a package from github sources (needs devtools)
    "install_github"|"github_package")
        InstallGithub "$@"
        ;;
    ##
    ## Install package dependencies from CRAN (needs devtools)
    "install_deps")
        InstallDeps
        ;;
    ##
    ## Install package dependencies from Bioconductor and CRAN (needs devtools)
    "install_bioc_deps")
        InstallBiocDeps
        ;;
    ##
    ## Run the actual tests, ie R CMD check
    "run_tests")
        RunTests
        ;;
    ##
    ## Dump information about installed packages
    "dump_sysinfo")
        DumpSysinfo
        ;;
    ##
    ## Dump build or check logs
    "dump_logs")
        DumpLogs
        ;;
    ##
    ## Dump selected build or check logs
    "dump_logs_by_extension")
        DumpLogsByExtension "$@"
        ;;
esac

#!/bin/bash
INSTALL_DIR=/usr/lib
VERSION=4.6.0
RELEASE=z3-4.6.0-x64-ubuntu-16.04
RELEASE_PATH=${INSTALL_DIR}/${RELEASE}

# Get Z3 release and unzip into /usr/lib/z3
sudo wget -P ${INSTALL_DIR} https://github.com/Z3Prover/z3/releases/download/z3-${VERSION}/${RELEASE}.zip \
  && sudo unzip ${RELEASE_PATH}.zip -d ${INSTALL_DIR} \
  && sudo rm ${RELEASE_PATH}.zip \
  && sudo mv ${RELEASE_PATH} ${INSTALL_DIR}/z3 \
  && sudo mv \
    ${INSTALL_DIR}/z3/bin/libz3.so \
    ${INSTALL_DIR}/z3/bin/libz3.a \
    ${INSTALL_DIR}/z3/bin/libz3java.so \
    ${INSTALL_DIR}/z3/bin/Microsoft.Z3.dll \
    ${INSTALL_DIR}/z3/bin/com.microsoft.z3.jar \
    ${INSTALL_DIR}




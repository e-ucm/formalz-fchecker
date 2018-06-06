#!/bin/bash
INSTALL_DIR=/usr/lib
RELEASE=z3-4.7.1-x64-ubuntu-16.04
RELEASE_PATH=${INSTALL_DIR}/${RELEASE}

# Get Z3-4.7.1 for Ubuntu 16.04 and unzip into /usr/lib/z3
sudo wget -P ${INSTALL_DIR} https://github.com/Z3Prover/z3/releases/download/z3-4.7.1/${RELEASE}.zip \
  && sudo unzip ${RELEASE_PATH}.zip -d ${RELEASE_PATH} \
  && sudo rm ${RELEASE_PATH}.zip \
  && sudo mv ${RELEASE_PATH}/${RELEASE}/ ${INSTALL_DIR}/z3 \
  && sudo rm -rf ${RELEASE_PATH} \
  && sudo mv \
    ${INSTALL_DIR}/z3/bin/libz3.so \
    ${INSTALL_DIR}/z3/bin/libz3.a \
    ${INSTALL_DIR}/z3/bin/libz3java.so \
    ${INSTALL_DIR}/z3/bin/Microsoft.Z3.dll \
    ${INSTALL_DIR}/z3/bin/Microsoft.Z3.xml \
    ${INSTALL_DIR}/z3/bin/com.microsoft.z3.jar \
    ${INSTALL_DIR}




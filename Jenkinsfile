pipeline {
    agent any
    environment {
        // Endevor Details
        ENDEVOR_CONNECTION="--port 6002 --protocol http --reject-unauthorized false"
        ENDEVOR_LOCATION="--instance ENDEVOR --env DEV --sys MARBLES --sub MARBLES --ccid JENKXX --comment JENKXX"
        ENDEVOR="$ENDEVOR_CONNECTION $ENDEVOR_LOCATION"
/*
        JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_131"
        PATH = "C:\\WINDOWS\\SYSTEM32;%JAVA_HOME%\\bin;C:\\Users\\yk892134\\AppData\\Roaming\\npm;C:\\Program Files\\nodejs;C:\\ZOWE;"
 
        // z/OSMF Connection Details
        ZOWE_OPT_HOST=credentials('eosHost')
        ZOWE_OPT_PORT="443"
        ZOWE_OPT_REJECT_UNAUTHORIZED=false
*/
        // File Master Plus Connection Details
        FMP="--port 6001 --protocol https --reject-unauthorized false"

        // CICS Connection Details
        CICS="--port 6000 --region-name CICS00A1"

    }
    stages {
        stage('Build') {
            steps {
                echo 'Building..'
                
                sh 'bright --version'
                sh 'zowe --version'
                sh 'pwd'
 //               sh 'sudo npm install gulp-cli -g'
  //              sh 'sudo npm install'
    //            sh 'sudo npm install gulp'
              
            }
        }
        stage('Deploy') {
            steps {
                echo 'Deploying....'
                sh 'zowe endevor --help'
                sh 'gulp --tasks'
                sh 'gulp build-cobol'
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
                sh 'gulp bind-n-grant'
            }
        }
    }
}
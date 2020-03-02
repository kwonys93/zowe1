pipeline {
    agent any
    environment {
        // Endevor Details
        ENDEVOR_CONNECTION="--port 6002 --protocol http --reject-unauthorized false"
        ENDEVOR_LOCATION="--instance ENDEVOR --env DEV --sys MARBLES --sub MARBLES --ccid JENKXX --comment JENKXX"
        ENDEVOR="$ENDEVOR_CONNECTION $ENDEVOR_LOCATION"

        JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.242.b08-0.el7_7.x86_64/jre"
        PATH = "/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/var/lib/zowe:/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.242.b08-0.el7_7.x86_64/jre:$PATH"
        
/* 
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
        stage('BUILD') {
          steps {
            parallel (
             "Build-cobol": { 
                echo 'Building cobol..'
                sh 'gulp build-cobol'
             },
             "Generate-cobol": { 
                echo 'Generating cobol..'
                sh 'gulp --tasks'
             },
             "Generate-lnk": { 
                echo 'Generating lnk..'
                sh "echo Jasmine"
             },
            )
          }
        }
        stage('COPYING') {
          steps {
            parallel (
             "Copy-load": { 
                echo 'Copying module to CICS env..'
                sh 'gulp copy-load'
             },
             "Copy-DBRM": { 
                echo 'Copying dbrm to db2..'
                sh 'gulp copy-dbrm'
             },
            )
          }
        }
        stage('DEPLOY') {
            steps {
              parallel (
                 "CICS-refresh": { 
                    echo 'New copying module in CICS..'
                    sh 'gulp cics-refresh'
                 },
                "Bind-n-grant": { 
                    echo 'Binding db2 plan and granting..'
                    sh 'gulp bind-n-grant'
                 },
              )  
            }
        }
        stage('TEST') {
            steps {
              parallel (
                "Test-data": { 
                    echo 'Testing data..'
                    sh 'gulp test-data'
                 },
                "Test-Validation": { 
                    echo 'Validating..'
                    sh 'gulp --tasks'
                },
              ) 
            }
        }
    }
}

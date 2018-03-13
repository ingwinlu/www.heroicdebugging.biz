pipeline {
  agent {
    dockerfile {
      filename 'Dockerfile'
    }
    
  }
  stages {
    stage('Build') {
      parallel {
        stage('Build') {
          agent {
            dockerfile {
              filename 'Dockerfile'
            }
            
          }
          steps {
            sh 'test'
          }
        }
        stage('Build v2') {
          steps {
            echo 'test'
          }
        }
      }
    }
  }
}
pipeline {
    agent any

    environment {
        AWS_REGION = 'ap-northeast-2'
        DOCKER_TAG = 'test'
    }

    stages {
        stage('소스코드 체크아웃') {
            steps {
                script {
                    deleteDir()
                    git branch: 'develop',
                        url: 'https://github.com/KTB-Sixmen/gitfolio_back_spring.git'
                }
            }
        }

        stage('환경 설정') {
            steps {
                script {
                    def envFile = '/var/lib/jenkins/environments/.env.back.dev'

                    if (fileExists(envFile)) {
                        sh """
                            cp ${envFile} .env
                            echo '환경 파일 복사 완료: ${envFile}'
                        """
                    } else {
                        error "환경 파일을 찾을 수 없습니다: ${envFile}"
                    }

                    // Docker Hub 로그인 - AI 파이프라인 방식으로 변경
                    withCredentials([usernamePassword(credentialsId: 'docker-credentials',
                                    usernameVariable: 'DOCKER_USER',
                                    passwordVariable: 'DOCKER_PASS')]) {
                        sh """
                            echo ${DOCKER_PASS} | docker login -u ${DOCKER_USER} --password-stdin
                        """
                    }
                }
            }
        }

        stage('Builder 이미지 빌드') {
            steps {
                script {
                    sh """
                        docker build \
                            -f dockerfile \
                            -t aida0/gitfolio_builder:${DOCKER_TAG} \
                            --platform linux/amd64 \
                            .

                        docker push aida0/gitfolio_builder:${DOCKER_TAG}
                    """
                }
            }
        }

        stage('모듈 빌드 및 배포') {
            matrix {
                axes {
                    axis {
                        name 'MODULE'
                        values 'auth', 'member', 'payment', 'resume', 'notification', 'chat'
                    }
                }
                stages {
                    stage('모듈 빌드') {
                        steps {
                            script {
                                def moduleConfig = [
                                    auth: [path: './gitfolio-auth', image: 'aida0/gitfolio_auth', index: '1'],
                                    member: [path: './gitfolio-member', image: 'aida0/gitfolio_member', index: '1'],
                                    payment: [path: './gitfolio-payment', image: 'aida0/gitfolio_payment', index: '2'],
                                    resume: [path: './gitfolio-resume', image: 'aida0/gitfolio_resume', index: '2'],
                                    notification: [path: './gitfolio-notification', image: 'aida0/gitfolio_notification', index: '3'],
                                    chat: [path: './gitfolio-chat', image: 'aida0/gitfolio_chat', index: '4']
                                ]

                                def config = moduleConfig[MODULE]

                                sh """
                                    docker build \
                                        -f ${config.path}/dockerfile \
                                        -t ${config.image}:${DOCKER_TAG} \
                                        ${config.path}

                                    docker push ${config.image}:${DOCKER_TAG}
                                """

                                // AWS 자격증명 처리를 AI 파이프라인 방식으로 변경
                                withCredentials([[$class: 'AmazonWebServicesCredentialsBinding',
                                                credentialsId: 'aws-credentials',
                                                accessKeyVariable: 'AWS_ACCESS_KEY_ID',
                                                secretKeyVariable: 'AWS_SECRET_ACCESS_KEY']]) {

                                    def instanceIds = sh(
                                        script: """
                                            aws ec2 describe-instances \
                                                --region ${AWS_REGION} \
                                                --filters 'Name=tag:Service,Values=back' \
                                                    'Name=tag:Index,Values=${config.index}' \
                                                    'Name=tag:Environment,Values=dev' \
                                                    'Name=tag:Type,Values=ec2' \
                                                    'Name=instance-state-name,Values=running' \
                                                --query 'Reservations[].Instances[].InstanceId' \
                                                --output text
                                        """,
                                        returnStdout: true
                                    ).trim()

                                    if (instanceIds) {
                                        sh """
                                            aws ssm send-command \
                                                --instance-ids "${instanceIds}" \
                                                --document-name "AWS-RunShellScript" \
                                                --comment "Deploying ${MODULE} module" \
                                                --parameters commands='
                                                    cd /home/ec2-user
                                                    docker-compose down -v --rmi all
                                                    docker-compose pull
                                                    docker-compose up -d
                                                ' \
                                                --timeout-seconds 600 \
                                                --region ${AWS_REGION}
                                        """
                                    } else {
                                        error "${MODULE} 모듈을 위한 EC2 인스턴스를 찾을 수 없습니다."
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    post {
        always {
            script {
                sh 'docker logout'
                sh 'rm -f .env'
            }
        }
        success {
            echo "파이프라인이 성공적으로 완료되었습니다."
        }
        failure {
            echo "파이프라인이 실패했습니다. 로그를 확인해주세요."
        }
    }
}
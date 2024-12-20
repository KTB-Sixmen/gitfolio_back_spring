pipeline {
    agent any

    environment {
        AWS_REGION = 'ap-northeast-2'
        ECR_REGISTRY = '727646500036.dkr.ecr.ap-northeast-2.amazonaws.com'
        DOCKER_TAG = 'dev'
    }

    stages {
        stage('소스코드 체크아웃') {
            steps {
                script {
                    // 이전 빌드 정리 및 새로운 소스코드 체크아웃
                    deleteDir()
                    git branch: 'develop',
                        url: 'https://github.com/KTB-Sixmen/gitfolio_back_spring.git'
                }
            }
        }

        stage('환경 설정') {
            steps {
                script {
                    // 환경 설정 파일 복사
                    def envFile = '/var/lib/jenkins/environments/.env.back.dev'

                    if (fileExists(envFile)) {
                        sh """
                            cp ${envFile} .env
                            echo '환경 파일 복사 완료: ${envFile}'
                        """
                    } else {
                        error "환경 파일을 찾을 수 없습니다: ${envFile}"
                    }

                    // AWS ECR 로그인 - Jenkins에 설정된 AWS 자격 증명 사용
                    withCredentials([[$class: 'AmazonWebServicesCredentialsBinding',
                                    credentialsId: 'aws-credentials',
                                    accessKeyVariable: 'AWS_ACCESS_KEY_ID',
                                    secretKeyVariable: 'AWS_SECRET_ACCESS_KEY']]) {
                        sh """
                            aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin ${ECR_REGISTRY}
                            echo 'ECR 로그인 완료'
                        """
                    }
                }
            }
        }

        stage('Builder 이미지 빌드') {
            steps {
                script {
                    // 로컬에서만 사용할 Builder 이미지 빌드
                    sh """
                        docker build \
                            -f dockerfile \
                            -t gitfolio_builder:${DOCKER_TAG} \
                            --platform linux/amd64 \
                            .
                        echo 'Builder 이미지 빌드 완료'
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
                                // 각 모듈별 설정 정보
                                def moduleConfig = [
                                    auth: [path: './gitfolio-auth', image: "${ECR_REGISTRY}/gitfolio/auth", index: '1'],
                                    member: [path: './gitfolio-member', image: "${ECR_REGISTRY}/gitfolio/member", index: '1'],
                                    payment: [path: './gitfolio-payment', image: "${ECR_REGISTRY}/gitfolio/payment", index: '2'],
                                    resume: [path: './gitfolio-resume', image: "${ECR_REGISTRY}/gitfolio/resume", index: '2'],
                                    notification: [path: './gitfolio-notification', image: "${ECR_REGISTRY}/gitfolio/notification", index: '3'],
                                    chat: [path: './gitfolio-chat', image: "${ECR_REGISTRY}/gitfolio/chat", index: '4']
                                ]

                                def config = moduleConfig[MODULE]

                                // 각 모듈 도커 이미지 빌드 및 ECR 푸시
                                sh """
                                    echo '${MODULE} 모듈 빌드 시작'

                                    docker build \
                                        -f ${config.path}/dockerfile \
                                        -t ${config.image}:${DOCKER_TAG} \
                                        --build-arg BUILDER_IMAGE=gitfolio_builder:${DOCKER_TAG} \
                                        ${config.path}

                                    docker push ${config.image}:${DOCKER_TAG}

                                    echo '${MODULE} 모듈 빌드 및 푸시 완료'
                                """

                                // AWS EC2 인스턴스 배포
                                withCredentials([[$class: 'AmazonWebServicesCredentialsBinding',
                                                credentialsId: 'aws-credentials',
                                                accessKeyVariable: 'AWS_ACCESS_KEY_ID',
                                                secretKeyVariable: 'AWS_SECRET_ACCESS_KEY']]) {

                                    // EC2 인스턴스 조회
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
                                        // SSM Run Command를 통한 배포 명령 실행
                                        sh """
                                            aws ssm send-command \
                                                --instance-ids "${instanceIds}" \
                                                --document-name "AWS-RunShellScript" \
                                                --comment "Deploying ${MODULE} module" \
                                                --parameters commands='
                                                    cd /home/ec2-user
                                                    docker-compose down -v --rmi all
                                                    docker builder prune -f --filter until=24h
                                                    docker image prune -f
                                                    aws ecr get-login-password --region ap-northeast-2 | docker login --username AWS --password-stdin 727646500036.dkr.ecr.ap-northeast-2.amazonaws.com
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
                // 빌드 완료 후 정리 작업
                sh """
                    docker-compose down -v
                    docker builder prune -f --filter until=24h
                    docker image prune -f
                    docker logout ${ECR_REGISTRY}
                    rm -f .env
                    echo '정리 작업 완료'
                """
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
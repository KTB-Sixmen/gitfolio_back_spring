pipeline {
    agent any

    environment {
        AWS_REGION = 'ap-northeast-2'
        ECR_REGISTRY = credentials('ecr-registry')
        DISCORD_CI_WEBHOOK = credentials('dev-discord-ci-webhook')
        DISCORD_CD_WEBHOOK = credentials('dev-discord-cd-webhook')
        DOCKER_TAG = 'prod'
    }

    stages {
        stage('소스코드 체크아웃') {
            steps {
                script {
                    deleteDir()
                    git branch: '48-Develop브랜치-푸시시-CI/CD-구현',
                        url: 'https://github.com/KTB-Sixmen/gitfolio_back_spring.git'
                }
            }
        }

        stage('환경 설정') {
            steps {
                script {
                    def envFile = '/var/lib/jenkins/environments/.env.back.prod'
                    if (fileExists(envFile)) {
                        sh """
                            cp \"${envFile}\" .env
                            echo \"환경 파일 복사 완료: \${envFile}\"

                            # URL 관련 환경변수
                            export REDIRECT_ONBOARDING_URL=\$(grep REDIRECT_ONBOARDING_URL .env | cut -d '=' -f2)
                            export REDIRECT_MAIN_URL=\$(grep REDIRECT_MAIN_URL .env | cut -d '=' -f2)
                            export MEMBER_SERVER_URL=\$(grep MEMBER_SERVER_URL .env | cut -d '=' -f2)
                            export PAYMENT_SERVER_URL=\$(grep PAYMENT_SERVER_URL .env | cut -d '=' -f2)
                            export NOTIFICATION_SERVER_URL=\$(grep NOTIFICATION_SERVER_URL .env | cut -d '=' -f2)
                            export AI_SERVER_URL=\$(grep AI_SERVER_URL .env | cut -d '=' -f2)
                            export PAYMENT_SUCCESS_REDIRECT_URL=\$(grep PAYMENT_SUCCESS_REDIRECT_URL .env | cut -d '=' -f2)

                            # gRPC 관련 환경변수
                            export MEMBER_GRPC_PORT=\$(grep MEMBER_GRPC_PORT .env | cut -d '=' -f2)

                            # 서버 포트 환경변수
                            export AUTH_SERVER_PORT=\$(grep AUTH_SERVER_PORT .env | cut -d '=' -f2)
                            export MEMBER_SERVER_PORT=\$(grep MEMBER_SERVER_PORT .env | cut -d '=' -f2)
                            export RESUME_SERVER_PORT=\$(grep RESUME_SERVER_PORT .env | cut -d '=' -f2)
                            export PAYMENT_SERVER_PORT=\$(grep PAYMENT_SERVER_PORT .env | cut -d '=' -f2)
                            export NOTIFICATION_SERVER_PORT=\$(grep NOTIFICATION_SERVER_PORT .env | cut -d '=' -f2)
                            export CHAT_SERVER_PORT=\$(grep CHAT_SERVER_PORT .env | cut -d '=' -f2)
                            # GitHub OAuth 관련 환경변수
                            export GH_CLIENT_ID=\$(grep GH_CLIENT_ID .env | cut -d '=' -f2)
                            export GH_CLIENT_SECRET=\$(grep GH_CLIENT_SECRET .env | cut -d '=' -f2)
                            export GH_REDIRECT_URI=\$(grep GH_REDIRECT_URI .env | cut -d '=' -f2)
                            export GH_API_TOKEN=\$(grep GH_API_TOKEN .env | cut -d '=' -f2)

                            # JWT 관련 환경변수
                            export JWT_SECRET_KEY=\$(grep JWT_SECRET_KEY .env | cut -d '=' -f2)
                            export ACCESS_TOKEN_EXPIRY=\$(grep ACCESS_TOKEN_EXPIRY .env | cut -d '=' -f2)
                            export REFRESH_TOKEN_EXPIRY=\$(grep REFRESH_TOKEN_EXPIRY .env | cut -d '=' -f2)

                            # Redis 관련 환경변수
                            export AUTH_REDIS_HOST=\$(grep AUTH_REDIS_HOST .env | cut -d '=' -f2)
                            export AUTH_REDIS_PORT=\$(grep AUTH_REDIS_PORT .env | cut -d '=' -f2)
                            export RESUME_REDIS_HOST=\$(grep RESUME_REDIS_HOST .env | cut -d '=' -f2)
                            export RESUME_REDIS_PORT=\$(grep RESUME_REDIS_PORT .env | cut -d '=' -f2)

                            # MySQL 관련 환경변수
                            export MEMBER_MYSQL_DB_HOST=\$(grep MEMBER_MYSQL_DB_HOST .env | cut -d '=' -f2)
                            export MEMBER_MYSQL_DB_PORT=\$(grep MEMBER_MYSQL_DB_PORT .env | cut -d '=' -f2)
                            export MEMBER_MYSQL_DB_NAME=\$(grep MEMBER_MYSQL_DB_NAME .env | cut -d '=' -f2)
                            export MEMBER_MYSQL_DB_PASSWORD=\$(grep MEMBER_MYSQL_DB_PASSWORD .env | cut -d '=' -f2)
                            export MEMBER_MYSQL_DB_USERNAME=\$(grep MEMBER_MYSQL_DB_USERNAME .env | cut -d '=' -f2)

                            export LIKE_MYSQL_DB_HOST=\$(grep LIKE_MYSQL_DB_HOST .env | cut -d '=' -f2)
                            export LIKE_MYSQL_DB_PORT=\$(grep LIKE_MYSQL_DB_PORT .env | cut -d '=' -f2)
                            export LIKE_MYSQL_DB_NAME=\$(grep LIKE_MYSQL_DB_NAME .env | cut -d '=' -f2)
                            export LIKE_MYSQL_DB_PASSWORD=\$(grep LIKE_MYSQL_DB_PASSWORD .env | cut -d '=' -f2)
                            export LIKE_MYSQL_DB_USERNAME=\$(grep LIKE_MYSQL_DB_USERNAME .env | cut -d '=' -f2)

                            export PAYMENT_MYSQL_DB_HOST=\$(grep PAYMENT_MYSQL_DB_HOST .env | cut -d '=' -f2)
                            export PAYMENT_MYSQL_DB_PORT=\$(grep PAYMENT_MYSQL_DB_PORT .env | cut -d '=' -f2)
                            export PAYMENT_MYSQL_DB_NAME=\$(grep PAYMENT_MYSQL_DB_NAME .env | cut -d '=' -f2)
                            export PAYMENT_MYSQL_DB_PASSWORD=\$(grep PAYMENT_MYSQL_DB_PASSWORD .env | cut -d '=' -f2)
                            export PAYMENT_MYSQL_DB_USERNAME=\$(grep PAYMENT_MYSQL_DB_USERNAME .env | cut -d '=' -f2)

                            export NOTIFICATION_MYSQL_DB_HOST=\$(grep NOTIFICATION_MYSQL_DB_HOST .env | cut -d '=' -f2)
                            export NOTIFICATION_MYSQL_DB_PORT=\$(grep NOTIFICATION_MYSQL_DB_PORT .env | cut -d '=' -f2)
                            export NOTIFICATION_MYSQL_DB_NAME=\$(grep NOTIFICATION_MYSQL_DB_NAME .env | cut -d '=' -f2)
                            export NOTIFICATION_MYSQL_DB_PASSWORD=\$(grep NOTIFICATION_MYSQL_DB_PASSWORD .env | cut -d '=' -f2)
                            export NOTIFICATION_MYSQL_DB_USERNAME=\$(grep NOTIFICATION_MYSQL_DB_USERNAME .env | cut -d '=' -f2)

                            # MongoDB 관련 환경변수
                            export MEMBER_MONGO_DB_URI=\$(grep MEMBER_MONGO_DB_URI .env | cut -d '=' -f2)
                            export RESUME_MONGO_DB_URI=\$(grep RESUME_MONGO_DB_URI .env | cut -d '=' -f2)
                            export CHAT_MONGO_DB_URI=\$(grep CHAT_MONGO_DB_URI .env | cut -d '=' -f2)

                            # S3 관련 환경변수
                            export S3_ACCESS_KEY=\$(grep S3_ACCESS_KEY .env | cut -d '=' -f2)
                            export S3_SECRET_KEY=\$(grep S3_SECRET_KEY .env | cut -d '=' -f2)
                            export S3_URL_PREFIX=\$(grep S3_URL_PREFIX .env | cut -d '=' -f2)

                            # Kakao Pay 관련 환경변수
                            export KAKAO_API_URL=\$(grep KAKAO_API_URL .env | cut -d '=' -f2)
                            export KAKAOPAY_SECRET_KEY=\$(grep KAKAOPAY_SECRET_KEY .env | cut -d '=' -f2)

                            # Kafka 관련 환경변수
                            export KAFKA_HOST1=\$(grep KAFKA_HOST1 .env | cut -d '=' -f2)
                            export KAFKA_PORT1=\$(grep KAFKA_PORT1 .env | cut -d '=' -f2)
                            export KAFKA_BROKER_ID=\$(grep KAFKA_BROKER_ID .env | cut -d '=' -f2)
                            export KAFKA_ZOOKEEPER_CONNECT=\$(grep KAFKA_ZOOKEEPER_CONNECT .env | cut -d '=' -f2)
                            export KAFKA_LISTENER_DOCKER=\$(grep KAFKA_LISTENER_DOCKER .env | cut -d '=' -f2)
                            export KAFKA_LISTENER_EXTERNAL=\$(grep KAFKA_LISTENER_EXTERNAL .env | cut -d '=' -f2)
                            export KAFKA_ADVERTISED_LISTENER_DOCKER=\$(grep KAFKA_ADVERTISED_LISTENER_DOCKER .env | cut -d '=' -f2)
                            export KAFKA_ADVERTISED_LISTENER_EXTERNAL=\$(grep KAFKA_ADVERTISED_LISTENER_EXTERNAL .env | cut -d '=' -f2)
                            export KAFKA_INTER_BROKER_LISTENER_NAME=\$(grep KAFKA_INTER_BROKER_LISTENER_NAME .env | cut -d '=' -f2)
                            export KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=\$(grep KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR .env | cut -d '=' -f2)
                                                    """
                                                } else {
                                                    error "환경 파일을 찾을 수 없습니다: ${envFile}"
                                                }

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
                                                        sh """
                                                            docker build \\
                                                                -f dockerfile \\
                                                                -t builder:${DOCKER_TAG} \\
                                                                --platform linux/amd64 \\
                                                                .

                                                            # ECR용 태그 추가
                                                            docker tag builder:${DOCKER_TAG} ${ECR_REGISTRY}/gitfolio/builder:${DOCKER_TAG}
                                                        """
                                                    }
                                                }
                                            }

                                            stage('모듈 빌드') {
                                                matrix {
                                                    axes {
                                                        axis {
                                                            name 'MODULE'
                                                            values 'auth', 'member', 'payment', 'resume', 'notification'
                                                        }
                                                    }
                                                    stages {
                                                        stage('모듈 빌드 및 푸시') {
                                                            steps {
                                                                script {
                                                                    def moduleConfig = [
                                                                        auth: [path: './gitfolio-auth', index: '1'],
                                                                        member: [path: './gitfolio-member', index: '1'],
                                                                        payment: [path: './gitfolio-payment', index: '2'],
                                                                        resume: [path: './gitfolio-resume', index: '2'],
                                                                        notification: [path: './gitfolio-notification', index: '3']
                                                                    ]

                                                                    def config = moduleConfig[MODULE]
                                                                    def imageTag = "${ECR_REGISTRY}/gitfolio/${MODULE}:${DOCKER_TAG}"

                                                                    sh """
                                                                        docker build \\
                                                                            -f ${config.path}/Dockerfile \\
                                                                            -t ${imageTag} \\
                                                                            --platform linux/amd64 \\
                                                                            --build-arg REDIRECT_ONBOARDING_URL=\${REDIRECT_ONBOARDING_URL} \
                                                                            --build-arg REDIRECT_MAIN_URL=\${REDIRECT_MAIN_URL} \
                                                                            --build-arg MEMBER_SERVER_URL=\${MEMBER_SERVER_URL} \
                                                                            --build-arg PAYMENT_SERVER_URL=\${PAYMENT_SERVER_URL} \
                                                                            --build-arg NOTIFICATION_SERVER_URL=\${NOTIFICATION_SERVER_URL} \
                                                                            --build-arg AI_SERVER_URL=\${AI_SERVER_URL} \
                                                                            --build-arg PAYMENT_SUCCESS_REDIRECT_URL=\${PAYMENT_SUCCESS_REDIRECT_URL} \
                                                                            --build-arg MEMBER_GRPC_PORT=\${MEMBER_GRPC_PORT} \
                                                                            --build-arg AUTH_SERVER_PORT=\${AUTH_SERVER_PORT} \
                                                                            --build-arg MEMBER_SERVER_PORT=\${MEMBER_SERVER_PORT} \
                                                                            --build-arg RESUME_SERVER_PORT=\${RESUME_SERVER_PORT} \
                                                                            --build-arg PAYMENT_SERVER_PORT=\${PAYMENT_SERVER_PORT} \
                                                                            --build-arg NOTIFICATION_SERVER_PORT=\${NOTIFICATION_SERVER_PORT} \
                                                                            --build-arg CHAT_SERVER_PORT=\${CHAT_SERVER_PORT} \
                                                                            --build-arg GH_CLIENT_ID=\${GH_CLIENT_ID} \
                                                                            --build-arg GH_CLIENT_SECRET=\${GH_CLIENT_SECRET} \
                                                                            --build-arg GH_REDIRECT_URI=\${GH_REDIRECT_URI} \
                                                                            --build-arg GH_API_TOKEN=\${GH_API_TOKEN} \
                                                                            --build-arg JWT_SECRET_KEY=\${JWT_SECRET_KEY} \
                                                                            --build-arg ACCESS_TOKEN_EXPIRY=\${ACCESS_TOKEN_EXPIRY} \
                                                                            --build-arg REFRESH_TOKEN_EXPIRY=\${REFRESH_TOKEN_EXPIRY} \
                                                                            --build-arg AUTH_REDIS_HOST=\${AUTH_REDIS_HOST} \
                                                                            --build-arg AUTH_REDIS_PORT=\${AUTH_REDIS_PORT} \
                                                                            --build-arg RESUME_REDIS_HOST=\${RESUME_REDIS_HOST} \
                                                                            --build-arg RESUME_REDIS_PORT=\${RESUME_REDIS_PORT} \
                                                                            --build-arg MEMBER_MYSQL_DB_HOST=\${MEMBER_MYSQL_DB_HOST} \
                                                                            --build-arg MEMBER_MYSQL_DB_PORT=\${MEMBER_MYSQL_DB_PORT} \
                                                                            --build-arg MEMBER_MYSQL_DB_NAME=\${MEMBER_MYSQL_DB_NAME} \
                                                                            --build-arg MEMBER_MYSQL_DB_PASSWORD=\${MEMBER_MYSQL_DB_PASSWORD} \
                                                                            --build-arg MEMBER_MYSQL_DB_USERNAME=\${MEMBER_MYSQL_DB_USERNAME} \
                                                                            --build-arg LIKE_MYSQL_DB_HOST=\${LIKE_MYSQL_DB_HOST} \
                                                                            --build-arg LIKE_MYSQL_DB_PORT=\${LIKE_MYSQL_DB_PORT} \
                                                                            --build-arg LIKE_MYSQL_DB_NAME=\${LIKE_MYSQL_DB_NAME} \
                                                                            --build-arg LIKE_MYSQL_DB_PASSWORD=\${LIKE_MYSQL_DB_PASSWORD} \
                                                                            --build-arg LIKE_MYSQL_DB_USERNAME=\${LIKE_MYSQL_DB_USERNAME} \
                                                                            --build-arg PAYMENT_MYSQL_DB_HOST=\${PAYMENT_MYSQL_DB_HOST} \
                                                                            --build-arg PAYMENT_MYSQL_DB_PORT=\${PAYMENT_MYSQL_DB_PORT} \
                                                                            --build-arg PAYMENT_MYSQL_DB_NAME=\${PAYMENT_MYSQL_DB_NAME} \
                                                                            --build-arg PAYMENT_MYSQL_DB_PASSWORD=\${PAYMENT_MYSQL_DB_PASSWORD} \
                                                                            --build-arg PAYMENT_MYSQL_DB_USERNAME=\${PAYMENT_MYSQL_DB_USERNAME} \
                                                                            --build-arg NOTIFICATION_MYSQL_DB_HOST=\${NOTIFICATION_MYSQL_DB_HOST} \
                                                                            --build-arg NOTIFICATION_MYSQL_DB_PORT=\${NOTIFICATION_MYSQL_DB_PORT} \
                                                                            --build-arg NOTIFICATION_MYSQL_DB_NAME=\${NOTIFICATION_MYSQL_DB_NAME} \
                                                                            --build-arg NOTIFICATION_MYSQL_DB_PASSWORD=\${NOTIFICATION_MYSQL_DB_PASSWORD} \
                                                                            --build-arg NOTIFICATION_MYSQL_DB_USERNAME=\${NOTIFICATION_MYSQL_DB_USERNAME} \
                                                                            --build-arg MEMBER_MONGO_DB_URI=\${MEMBER_MONGO_DB_URI} \
                                                                            --build-arg RESUME_MONGO_DB_URI=\${RESUME_MONGO_DB_URI} \
                                                                            --build-arg CHAT_MONGO_DB_URI=\${CHAT_MONGO_DB_URI} \
                                                                            --build-arg S3_ACCESS_KEY=\${S3_ACCESS_KEY} \
                                                                            --build-arg S3_SECRET_KEY=\${S3_SECRET_KEY} \
                                                                            --build-arg S3_URL_PREFIX=\${S3_URL_PREFIX} \
                                                                            --build-arg KAKAO_API_URL=\${KAKAO_API_URL} \
                                                                            --build-arg KAKAOPAY_SECRET_KEY=\${KAKAOPAY_SECRET_KEY} \
                                                                            --build-arg KAFKA_HOST1=\${KAFKA_HOST1} \
                                                                            --build-arg KAFKA_PORT1=\${KAFKA_PORT1} \
                                                                            --build-arg KAFKA_BROKER_ID=\${KAFKA_BROKER_ID} \
                                                                            --build-arg KAFKA_ZOOKEEPER_CONNECT=\${KAFKA_ZOOKEEPER_CONNECT} \
                                                                            --build-arg KAFKA_LISTENER_DOCKER=\${KAFKA_LISTENER_DOCKER} \
                                                                            --build-arg KAFKA_LISTENER_EXTERNAL=\${KAFKA_LISTENER_EXTERNAL} \
                                                                            --build-arg KAFKA_ADVERTISED_LISTENER_DOCKER=\${KAFKA_ADVERTISED_LISTENER_DOCKER} \
                                                                            --build-arg KAFKA_ADVERTISED_LISTENER_EXTERNAL=\${KAFKA_ADVERTISED_LISTENER_EXTERNAL} \
                                                                            --build-arg KAFKA_INTER_BROKER_LISTENER_NAME=\${KAFKA_INTER_BROKER_LISTENER_NAME} \
                                                                            --build-arg KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=\${KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR} \
                                                                            .

                                                                        docker push ${imageTag}
                                                                    """
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
                                                    sh """
                                                        docker builder prune -f --filter until=24h
                                                        docker image prune -f
                                                        rm -f .env
                                                    """
                                                }
                                            }
                                        }
                                    }
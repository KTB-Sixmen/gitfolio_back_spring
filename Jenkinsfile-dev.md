pipeline {
agent any

    environment {
        AWS_REGION = 'ap-northeast-2'
        ECR_REGISTRY = credentials('ecr-registry')
        DISCORD_CI_WEBHOOK = credentials('dev-discord-ci-webhook')
        DISCORD_CD_WEBHOOK = credentials('dev-discord-cd-webhook')
        DOCKER_TAG = 'dev'
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
                        docker build \
                            -f dockerfile \
                            -t builder:${DOCKER_TAG} \
                            --platform linux/amd64 \
                            .

                        # ECR용 태그 추가
                        docker tag builder:${DOCKER_TAG} ${ECR_REGISTRY}/gitfolio/builder:${DOCKER_TAG}
                    """
                }
            }
        }

        stage('모듈 빌드 및 배포') {
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
                                    docker build \
                                        -f ${config.path}/Dockerfile \
                                        -t ${imageTag} \
                                        --platform linux/amd64 \
                                        .

                                    docker push ${imageTag}
                                """

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
                                                    export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID}
                                                    export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY}
                                                    export AWS_DEFAULT_REGION=ap-northeast-2
                                                    aws ecr get-login-password --region ap-northeast-2 | docker login --username AWS --password-stdin 727646500036.dkr.ecr.ap-northeast-2.amazonaws.com
                                                    docker-compose down -v --rmi all
                                                    docker builder prune -f --filter until=24h
                                                    docker image prune -f
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
                sh """
                    docker builder prune -f --filter until=24h
                    docker image prune -f
                    rm -f .env
                """
            }
        }
            success {
            discordSend description: "dev 백엔드 CI 파이프라인 성공",
            footer: "Jenkins Pipeline Success",
            link: env.BUILD_URL,
            result: currentBuild.currentResult,
            title: JOB_NAME,
            webhookURL: DISCORD_CI_WEBHOOK
            }
            failure {
            discordSend description: "dev 백엔드 CI 파이프라인 실패",
            footer: "Jenkins Pipeline Failed",
            link: env.BUILD_URL,
            result: currentBuild.currentResult,
            title: JOB_NAME,
            webhookURL: DISCORD_CI_WEBHOOK
            }
            


    }
}
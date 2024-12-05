FROM gradle:7-jdk17 AS builder

# URL 관련 ARG
ARG REDIRECT_ONBOARDING_URL
ARG REDIRECT_MAIN_URL
ARG MEMBER_SERVER_URL
ARG PAYMENT_SERVER_URL
ARG NOTIFICATION_SERVER_URL
ARG AI_SERVER_URL
ARG PAYMENT_SUCCESS_REDIRECT_URL

# gRPC와 Server Port ARG
ARG MEMBER_GRPC_PORT
ARG AUTH_SERVER_PORT
ARG MEMBER_SERVER_PORT
ARG RESUME_SERVER_PORT
ARG PAYMENT_SERVER_PORT
ARG NOTIFICATION_SERVER_PORT
ARG CHAT_SERVER_PORT

# GitHub OAuth 관련 ARG
ARG GH_CLIENT_ID
ARG GH_CLIENT_SECRET
ARG GH_REDIRECT_URI
ARG GH_API_TOKEN

# JWT 관련 ARG
ARG JWT_SECRET_KEY
ARG ACCESS_TOKEN_EXPIRY
ARG REFRESH_TOKEN_EXPIRY

# Redis 관련 ARG
ARG AUTH_REDIS_HOST
ARG AUTH_REDIS_PORT
ARG RESUME_REDIS_HOST
ARG RESUME_REDIS_PORT

# MySQL 관련 ARG
ARG MEMBER_MYSQL_DB_HOST
ARG MEMBER_MYSQL_DB_PORT
ARG MEMBER_MYSQL_DB_NAME
ARG MEMBER_MYSQL_DB_PASSWORD
ARG MEMBER_MYSQL_DB_USERNAME
ARG LIKE_MYSQL_DB_HOST
ARG LIKE_MYSQL_DB_PORT
ARG LIKE_MYSQL_DB_NAME
ARG LIKE_MYSQL_DB_PASSWORD
ARG LIKE_MYSQL_DB_USERNAME
ARG PAYMENT_MYSQL_DB_HOST
ARG PAYMENT_MYSQL_DB_PORT
ARG PAYMENT_MYSQL_DB_NAME
ARG PAYMENT_MYSQL_DB_PASSWORD
ARG PAYMENT_MYSQL_DB_USERNAME
ARG NOTIFICATION_MYSQL_DB_HOST
ARG NOTIFICATION_MYSQL_DB_PORT
ARG NOTIFICATION_MYSQL_DB_NAME
ARG NOTIFICATION_MYSQL_DB_PASSWORD
ARG NOTIFICATION_MYSQL_DB_USERNAME

# MongoDB 관련 ARG
ARG MEMBER_MONGO_DB_USERNAME
ARG MEMBER_MONGO_DB_PORT
ARG MEMBER_MONGO_DB_DATABASE
ARG RESUME_MONGO_DB_USERNAME
ARG RESUME_MONGO_DB_PORT
ARG RESUME_MONGO_DB_DATABASE
ARG CHAT_MONGO_DB_USERNAME
ARG CHAT_MONGO_DB_PORT
ARG CHAT_MONGO_DB_DATABASE

# S3 관련 ARG
ARG S3_ACCESS_KEY
ARG S3_SECRET_KEY
ARG S3_URL_PREFIX

# Kakao Pay 관련 ARG
ARG KAKAO_API_URL
ARG KAKAOPAY_SECRET_KEY

# Kafka 관련 ARG
ARG KAFKA_HOST1
ARG KAFKA_PORT1
ARG KAFKA_BROKER_ID
ARG KAFKA_ZOOKEEPER_CONNECT
ARG KAFKA_LISTENER_DOCKER
ARG KAFKA_LISTENER_EXTERNAL
ARG KAFKA_ADVERTISED_LISTENER_DOCKER
ARG KAFKA_ADVERTISED_LISTENER_EXTERNAL
ARG KAFKA_INTER_BROKER_LISTENER_NAME
ARG KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR

# ARG를 ENV로 변환
ENV REDIRECT_ONBOARDING_URL=${REDIRECT_ONBOARDING_URL}
ENV REDIRECT_MAIN_URL=${REDIRECT_MAIN_URL}
ENV MEMBER_SERVER_URL=${MEMBER_SERVER_URL}
ENV PAYMENT_SERVER_URL=${PAYMENT_SERVER_URL}
ENV NOTIFICATION_SERVER_URL=${NOTIFICATION_SERVER_URL}
ENV AI_SERVER_URL=${AI_SERVER_URL}
ENV PAYMENT_SUCCESS_REDIRECT_URL=${PAYMENT_SUCCESS_REDIRECT_URL}

ENV MEMBER_GRPC_PORT=${MEMBER_GRPC_PORT}
ENV AUTH_SERVER_PORT=${AUTH_SERVER_PORT}
ENV MEMBER_SERVER_PORT=${MEMBER_SERVER_PORT}
ENV RESUME_SERVER_PORT=${RESUME_SERVER_PORT}
ENV PAYMENT_SERVER_PORT=${PAYMENT_SERVER_PORT}
ENV NOTIFICATION_SERVER_PORT=${NOTIFICATION_SERVER_PORT}
ENV CHAT_SERVER_PORT=${CHAT_SERVER_PORT}

ENV GH_CLIENT_ID=${GH_CLIENT_ID}
ENV GH_CLIENT_SECRET=${GH_CLIENT_SECRET}
ENV GH_REDIRECT_URI=${GH_REDIRECT_URI}
ENV GH_API_TOKEN=${GH_API_TOKEN}

ENV JWT_SECRET_KEY=${JWT_SECRET_KEY}
ENV ACCESS_TOKEN_EXPIRY=${ACCESS_TOKEN_EXPIRY}
ENV REFRESH_TOKEN_EXPIRY=${REFRESH_TOKEN_EXPIRY}

ENV AUTH_REDIS_HOST=${AUTH_REDIS_HOST}
ENV AUTH_REDIS_PORT=${AUTH_REDIS_PORT}
ENV RESUME_REDIS_HOST=${RESUME_REDIS_HOST}
ENV RESUME_REDIS_PORT=${RESUME_REDIS_PORT}

ENV MEMBER_MYSQL_DB_HOST=${MEMBER_MYSQL_DB_HOST}
ENV MEMBER_MYSQL_DB_PORT=${MEMBER_MYSQL_DB_PORT}
ENV MEMBER_MYSQL_DB_NAME=${MEMBER_MYSQL_DB_NAME}
ENV MEMBER_MYSQL_DB_PASSWORD=${MEMBER_MYSQL_DB_PASSWORD}
ENV MEMBER_MYSQL_DB_USERNAME=${MEMBER_MYSQL_DB_USERNAME}
ENV LIKE_MYSQL_DB_HOST=${LIKE_MYSQL_DB_HOST}
ENV LIKE_MYSQL_DB_PORT=${LIKE_MYSQL_DB_PORT}
ENV LIKE_MYSQL_DB_NAME=${LIKE_MYSQL_DB_NAME}
ENV LIKE_MYSQL_DB_PASSWORD=${LIKE_MYSQL_DB_PASSWORD}
ENV LIKE_MYSQL_DB_USERNAME=${LIKE_MYSQL_DB_USERNAME}
ENV PAYMENT_MYSQL_DB_HOST=${PAYMENT_MYSQL_DB_HOST}
ENV PAYMENT_MYSQL_DB_PORT=${PAYMENT_MYSQL_DB_PORT}
ENV PAYMENT_MYSQL_DB_NAME=${PAYMENT_MYSQL_DB_NAME}
ENV PAYMENT_MYSQL_DB_PASSWORD=${PAYMENT_MYSQL_DB_PASSWORD}
ENV PAYMENT_MYSQL_DB_USERNAME=${PAYMENT_MYSQL_DB_USERNAME}
ENV NOTIFICATION_MYSQL_DB_HOST=${NOTIFICATION_MYSQL_DB_HOST}
ENV NOTIFICATION_MYSQL_DB_PORT=${NOTIFICATION_MYSQL_DB_PORT}
ENV NOTIFICATION_MYSQL_DB_NAME=${NOTIFICATION_MYSQL_DB_NAME}
ENV NOTIFICATION_MYSQL_DB_PASSWORD=${NOTIFICATION_MYSQL_DB_PASSWORD}
ENV NOTIFICATION_MYSQL_DB_USERNAME=${NOTIFICATION_MYSQL_DB_USERNAME}

ENV MEMBER_MONGO_DB_USERNAME=${MEMBER_MONGO_DB_USERNAME}
ENV MEMBER_MONGO_DB_PORT=${MEMBER_MONGO_DB_PORT}
ENV MEMBER_MONGO_DB_DATABASE=${MEMBER_MONGO_DB_DATABASE}
ENV RESUME_MONGO_DB_USERNAME=${RESUME_MONGO_DB_USERNAME}
ENV RESUME_MONGO_DB_PORT=${RESUME_MONGO_DB_PORT}
ENV RESUME_MONGO_DB_DATABASE=${RESUME_MONGO_DB_DATABASE}
ENV CHAT_MONGO_DB_USERNAME=${CHAT_MONGO_DB_USERNAME}
ENV CHAT_MONGO_DB_PORT=${CHAT_MONGO_DB_PORT}
ENV CHAT_MONGO_DB_DATABASE=${CHAT_MONGO_DB_DATABASE}

ENV S3_ACCESS_KEY=${S3_ACCESS_KEY}
ENV S3_SECRET_KEY=${S3_SECRET_KEY}
ENV S3_URL_PREFIX=${S3_URL_PREFIX}

ENV KAKAO_API_URL=${KAKAO_API_URL}
ENV KAKAOPAY_SECRET_KEY=${KAKAOPAY_SECRET_KEY}

ENV KAFKA_HOST1=${KAFKA_HOST1}
ENV KAFKA_PORT1=${KAFKA_PORT1}
ENV KAFKA_BROKER_ID=${KAFKA_BROKER_ID}
ENV KAFKA_ZOOKEEPER_CONNECT=${KAFKA_ZOOKEEPER_CONNECT}
ENV KAFKA_LISTENER_DOCKER=${KAFKA_LISTENER_DOCKER}
ENV KAFKA_LISTENER_EXTERNAL=${KAFKA_LISTENER_EXTERNAL}
ENV KAFKA_ADVERTISED_LISTENER_DOCKER=${KAFKA_ADVERTISED_LISTENER_DOCKER}
ENV KAFKA_ADVERTISED_LISTENER_EXTERNAL=${KAFKA_ADVERTISED_LISTENER_EXTERNAL}
ENV KAFKA_INTER_BROKER_LISTENER_NAME=${KAFKA_INTER_BROKER_LISTENER_NAME}
ENV KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=${KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR}

WORKDIR /gitfolio_back
# 이제 나머지 빌드 설정 파일들을 복사합니다
COPY settings.gradle build.gradle ./
# Gradle Wrapper 파일들을 먼저 복사합니다
COPY gradle gradle
COPY gradlew .
COPY gradlew.bat .
# COPY gradle-wrapper.properties gradle/wrapper/gradle-wrapper.properties

# gradle wrapper jar 파일이 없다면 gradle wrapper를 새로 생성합니다
RUN if [ ! -f gradle/wrapper/gradle-wrapper.jar ]; then \
    gradle wrapper; \
    fi



# COPY settings.gradle build.gradle ./
# COPY gradle ./gradle
# COPY gradlew ./

RUN chmod +x gradlew && \
    ./gradlew dependencies

COPY . .

RUN ./gradlew clean build -x test && \
    cp */build/libs/*-SNAPSHOT.jar build && \
    printenv > /gitfolio_back/build/.env

    #     cp .env build && \


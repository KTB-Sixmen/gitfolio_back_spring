# 빌드 스테이지
FROM gradle:7-jdk17 AS builder

WORKDIR /gitfolio_back

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

RUN chmod +x gradlew && \
    ./gradlew dependencies

COPY . .

RUN ./gradlew clean build -x test && \
    cp .env build && \
    cp */build/libs/*-SNAPSHOT.jar build
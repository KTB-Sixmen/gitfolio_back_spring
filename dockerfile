FROM gradle:7-jdk17 AS builder

WORKDIR /gitfolio_back

COPY settings.gradle build.gradle ./
COPY gradle ./gradle
COPY gradlew ./

RUN chmod +x gradlew && \
    ./gradlew dependencies

COPY . .

RUN ./gradlew clean build -x test && \
    cp .env build && \
    cp */build/libs/*-SNAPSHOT.jar build
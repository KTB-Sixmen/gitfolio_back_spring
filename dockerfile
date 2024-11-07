FROM gradle:7-jdk17 AS builder

WORKDIR /gitfolio_back

COPY settings.gradle build.gradle ./
COPY gradle ./gradle
COPY gradlew ./

RUN chmod +x gradlew

RUN ./gradlew dependencies

COPY . .

RUN ./gradlew clean build -x test

RUN cp .env build
RUN cp */build/libs/*-SNAPSHOT.jar build
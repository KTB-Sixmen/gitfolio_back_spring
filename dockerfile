FROM gradle:7-jdk17 AS builder

WORKDIR /gitfolio_back

COPY settings.gradle build.gradle ./
COPY gradle ./gradle
COPY gradlew ./

COPY . .

RUN chmod +x gradlew && \
    ./gradlew dependencies

# RUN ./gradlew clean build -x test && \
#     cp .env build && \
#     cp */build/libs/*-SNAPSHOT.jar build

RUN ./gradlew clean build -x test && \
    test -f .env && cp .env build || true && \
    cp */build/libs/*-SNAPSHOT.jar build
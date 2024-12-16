# 빌드 스테이지
FROM gradle:7-jdk17 AS builder

WORKDIR /gitfolio_back

# 그래들 설정 파일 복사
COPY settings.gradle build.gradle ./
COPY gradle ./gradle
COPY gradlew ./

# 의존성 다운로드
RUN chmod +x gradlew && \
    ./gradlew dependencies

# 소스 코드와 환경 파일 복사
COPY . .
# 이 시점에서 Jenkins가 복사한 .env 파일이 사용됨

# 빌드 실행 및 환경 파일 복사
RUN ./gradlew clean build -x test && \
    cp .env build/ && \
    cp */build/libs/*-SNAPSHOT.jar build/
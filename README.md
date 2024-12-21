# 깃허브로 완성하는 나만의 이력서, Gitfolio

</br>

</br>

### 🔖 프로젝트 개요
  - **주제** : GitHub 데이터를 활용한 LLM 기반 이력서 자동 생성 서비스
  - **대상** : 이력서 작성에 어려움을 느끼는 예비 개발자

</br>

### 🎯 목표
  - **AI 기반 이력서 생성 및 수정**: 사용자의 GitHub 데이터를 분석하여 실제 기업에 제출 가능한 이력서를 AI와 함께 손쉽게 제작 및 수정
  - **커뮤니티 환경 제공**: 사용자 간 이력서를 공유하고, 피드백을 주고받을 수 있는 협력의 장 제공


</br>

### 📚 기술 스택

|파트|프레임워크 & 라이브러리|
|---|---|
|Frontend|`Next.js`, `Typescript`, `TailWindCSS`, `Zustand`|
|Backend|`Java`, `Spring Boot`, `Spring Security`, `Redis`, `Kafka`, `Spring Data Jpa`|
|AI/ML|`Fast API`, `GPT-4o`, `LangChain`, `ollama`, `onnx`|
|Infra|`Docker`, `AWS`, `Ansible`, `Terraform`, `Prometheus`, `Grafana`, `Kubernetes`, `Jenkins`|
|Collaborative|`Notion`, `Figma`, `Github`, `Discord`|


</br>


### 🔗 주요 기능

  #### 1️⃣ 회원 및 인증 관리 시스템
  - GitHub 소셜 로그인을 통해 회원 가입 및 로그인을 할 수 있습니다.
  - 마이페이지에서 회원 정보를 관리할 수 있습니다.

  #### 2️⃣ 이력서 생성 및 수정
  - 사용자의 GitHub 데이터를 분석해 AI가 이력서를 자동으로 생성해줍니다.
  - 이력서는 3가지의 템플릿 중 1개를 골라 해당 형식으로 이력서를 작성할 수 있습니다.
  - 수정하고 싶은 부분을 사용자가 드래그 하고 요청사항을 입력하면 AI가 이력서를 수정해줍니다.
  - 직접 수정하고 싶은 부분은 사용자가 직접 수정할 수 있습니다.
  - 완성된 이력서는 PDF 파일로 다운로드 받을 수 있습니다.

  #### 3️⃣ 커뮤니티
  - 이력서 허브에서 사용자들은 이력서를 공유하고 피드백 받을 수 있습니다.
  - 관심 있는 이력서는 좋아요를 누를 수 있고, 좋아요 누른 이력서만 조회할 수 있습니다.
  - 직군, 학력, 조회수 순, 좋아요 순 등 다양한 필터로 이력서들을 조회해 볼 수 있습니다.

  #### 4️⃣ 결제
  - 카카오페이 API를 통해 사용자는 정기 결제를 진행해 PRO 플랜으로 사용할 수 있습니다.
  - FREE 플랜 사용자는 하루에 3회의 사용권만 사용할 수 있고 몇가지 제약사항이 존재합니다.
  - 매일 자정에 백엔드에서는 배치작업을 통해 만료된 결제를 처리하고, FREE 플랜 사용자의 사용권을 초기화합니다.

</br>

### 🌈 개선 사항

  #### 1️⃣ 배치 성능 최적화 - <ins>실행 시간 약 84% 감소</ins>

  </br>

  **문제 상황**
  - 정기 결제 및 사용권 초기화를 배치로 처리하고 있는데, 현재는 배치 로직도 엄청 복잡하지 않고, 데이터 양도 많지 않지만, 나중에 데이터가 많아지게될 경우 성능 저하가 우려됨

  **1. 순차처리 → 병렬처리**
  - 기존에 순차적으로 진행하던 작업을 병렬로 처리
  - 성능 개선은 확실히 됐지만, 로그에 각각의 update 쿼리가 개별적으로 실행되고 있는 것을 확인

  **2. JPA → JDBC 전환**
  - 쿼리가 개별적으로 실행되면서 배치로 일괄처리 하는것의 이점을 전혀 살리지 못하고 있다고 판단
  - JPA의 객체 매핑 기능이 매우 편리하긴 하나, 트랜잭션 관리, 변경 감지 등의 추가적인 기능이 필요하지 않고, 오히려 오버헤드가 발생해 성능이 저하되는 것을 확인
  - 기존 JpaItemWriter을 JdbcBatchItemWriter로 변경해 설정해둔 Chunk Size를 그대로 반영해 Batch Update가 되도록 수정해 성능을 개선

  **결과**
  
  <img width="742" alt="스크린샷 2024-12-17 15 33 20" src="https://github.com/user-attachments/assets/ef411e3b-e757-47df-8572-8102f83dc10d" />

</br>  

  #### 2️⃣ 알림 모듈 처리량 개선 - <ins>TPS 약 2배 향상</ins>

</br>
  
  **문제 상황**
  - Resume 모듈에서 좋아요를 누르거나, 댓글을 작성하는 경우, Notification 모듈로 알림 생성 요청을 보내는데, 동기로 요청을 보내다 보니 요청이 많아질수록 응답속도와 처리량이 저하됨
  - 알림 시스템 특성상 다수의 사용자와 대규모 트래픽을 처리해야할 것으로 예상되는데, 기존 동기 방식으로는 감당할 수 없을 것 같아 테스트 진행

</br>
  
  **JMeter 쓰레드 그룹 설정**  

  <img width="343" alt="스크린샷 2024-12-03 16 35 00" src="https://github.com/user-attachments/assets/fce290f1-b10e-4de5-9594-0bffb3143263">

</br>
  
  **1. WebClient(blocking)**  
  
  <img width="821" alt="스크린샷 2024-12-03 16 37 00" src="https://github.com/user-attachments/assets/e2b810d2-9524-4c30-8518-2bc7ffca16c3">
<img width="1018" alt="스크린샷 2024-12-03 16 35 32" src="https://github.com/user-attachments/assets/857e290a-99dc-4de4-bc47-a8be415ccd8c">

**평균 응답속도** : 1304ms  
**99% 선 (최악의 경우)** : 4155ms  
**TPS** : 365.4/sec  

</br>
  
  **2. WebClient(non-blocking)**  
  
  <img width="817" alt="스크린샷 2024-12-03 16 52 31" src="https://github.com/user-attachments/assets/2da3d7af-d668-4d46-a1ca-a9639f75c9d3">
<img width="1508" alt="스크린샷 2024-12-03 16 52 44" src="https://github.com/user-attachments/assets/63562a46-24f8-4ee5-9ff2-dc5d51e3af07">

**평균 응답속도** : 987ms  
**99% 선 (최악의 경우)** : 4162ms  
**TPS** : 477.8/sec  

</br>
  
  **3. Kafka**
  
  <img width="815" alt="스크린샷 2024-12-03 16 47 37" src="https://github.com/user-attachments/assets/ae488926-c6e6-46b5-b9ff-6091cd53a2ef">
<img width="1507" alt="스크린샷 2024-12-03 16 48 15" src="https://github.com/user-attachments/assets/bf180818-607f-49b6-9c57-162a157a5c96">

**평균 응답속도** : 608ms  
**99% 선 (최악의 경우)** : 2245ms  
**TPS** : 761.3/sec  

</br>

**결론**
확장성 측면에서는 Kafka가 훨씬 더 좋을 것으로 예상되나, 트래픽이 엄청나게 몰리는 것이 아니고 비용적인 측면을 고려해야 한다면 WebClient 비동기 요청도 괜찮을 것 같음

---

</br>

  #### 3️⃣ GitHub API를 통해 사용자 레포지토리 목록 조회 성능 개선 - <ins>응답속도 약 31배 개선</ins>
  
</br>

  **문제 상황**
  - GitHub API를 사용해서 조회하는 것인데 응답시간이 상당히 오래 걸리던 상황
  - 처음 회원 가입한 회원인 경우, 온보딩 페이지에서 다른 입력값을 입력하고 있을 때 미리 호출해 놓기 때문에 사용자가 응답을 기다리는 시간이 없었음
  - 커뮤니티에서 "새 이력서 만들기"를 클릭하면 레포지토리 조회하는데 약 3~4초의 시간을 사용자가 기다려야 하는 상황이 발생
  - 그러나, 시간이 오래 소요되는 부분이 GitHub API였기 때문에 해당 로직을 수정하거나 개선할 수 없음

</br>

  **문제 해결**
  - 레포지토리 목록은 변경 가능성이 매우 적고, 짧은 시간 사이에 이력서에 넣을 정도의 레포지토리가 생성되는 것이 불가능하다고 판단
  - 해당 데이터를 캐싱해놓고 쓴다면 이점이 매우 클 것으로 예상
  - 1시간정도 해당 데이터를 Redis에 캐싱하므로 첫 요청 말고는 사용자가 기다리는 시간이 거의 없도록 개선

</br>

**<캐싱 하기 전>** - 응답시간 <ins>약 3600ms</ins>
<img width="1287" alt="스크린샷 2024-11-16 16 22 52" src="https://github.com/user-attachments/assets/2baeb23d-2e2a-4486-9e73-009e836743a3">

**<캐싱 이후>** - 응답시간 <ins>약 120ms</ins>
<img width="1295" alt="스크린샷 2024-11-16 16 23 04" src="https://github.com/user-attachments/assets/bd6d4956-4b41-48cf-b9cb-0306e8b6cc85">

---

</br>

  #### 4️⃣ 검색 필터 조회 성능 개선 - <ins>약 55.4% 개선</ins>
  - 캐싱 시도
    - 필터 자체를 캐싱하자니 Cache Hit 확률이 너무 낮아 성능 개선이 안됨
    - 각 이력서 정보를 캐싱해봤으나, 필터 검색에서 쿼리 생성하는 부분에서 성능이 매우 저하되는 것이라 개선 불가
  - 필터 검색 로직 자체를 최적화 시도
    - 코드를 다시 보니 regex를 쓰는 부분이 많았는데 해당 방식은 전체 데이터를 스캔하므로 성능이 저하되는 것 확인
    - regex를 is로 변경하고, 배열 내 개별 요소에 대해 비교할 때 elemMatch를 사용하는 것으로 변경해 일치 항목을 찾으면 스캔을 중단하도록 개선
    - 불필요한 쿼리 조건 제거 및 단순화
   
</br>

  **<개선 전>**
  <img width="1337" alt="스크린샷 2024-11-12 11 06 11" src="https://github.com/user-attachments/assets/c8d814f9-6273-4dc6-86c2-750fbef3c266">
<img width="1338" alt="스크린샷 2024-11-12 11 55 36" src="https://github.com/user-attachments/assets/090f7855-1126-44f4-8f0b-520a9c4dfac2">
<img width="1335" alt="스크린샷 2024-11-12 13 30 15" src="https://github.com/user-attachments/assets/305cc866-441a-4364-991f-79983b0294d6">

  **<개선 후>**
  <img width="1338" alt="스크린샷 2024-11-12 13 56 30" src="https://github.com/user-attachments/assets/17f06a25-d0d9-4939-a7d5-0cd354680550">
<img width="1337" alt="스크린샷 2024-11-12 11 53 50" src="https://github.com/user-attachments/assets/0a5ab0d1-569a-4c08-b5a7-c83ec3eeabf0">
<img width="1334" alt="스크린샷 2024-11-12 13 53 27" src="https://github.com/user-attachments/assets/7279f68c-686d-48f9-bfa1-52d9a901905f">

</br>



</br>

### 🚀 트러블 슈팅

  #### 1️⃣ **조회수 집계 구현시 동일 사용자가 새로고침할 때마다 계속해서 조회수가 증가**
  - 특정한 조건 없이 해당 이력서를 조회할 때마다 조회수가 올라가는 것은 조회수 정보의 신뢰성을 떨어뜨릴 수 있다고 판단
  - Redis에 이력서 ID와 사용자 IP 주소를 합친 문자열을 1시간 동안 저장하도록 설정해 해결
    - 처음 이력서를 조회한 경우 조회수가 증가하지만 그 이후 1시간 동안은 동일 이력서를 조회해도 조회수 집계되지 않음
    - Redis의 TTL설정을 통해 편리하게 관리 가능
  #### 2️⃣ **MongoDB에서 페이지네이션 적용시 10만건 데이터 기준 총 8000 페이지에서 약 4000 페이지부터 메모리 관련 에러 발생**
  - MongoDB는 기본적으로 쿼리나 집계 파이프라인 수행시 메모리에서만 작업을 처리하는 것을 확인
  - 이 과정에서 대량의 문서를 정렬하고 집계하다보니 메모리를 초과해 에러가 발생하던 것
    - 특히 페이징 처리시 전체 페이지와 전체 문서 개수 집계 과정에서 메모리가 초과됨
  - allowDiskUse(true) 설정을 통해 디스크를 추가로 사용해 연산을 완료할 수 있도록 허용해 해결
  #### 3️⃣ **배치 작업 시 Kafka와 Spring Batch의 트랜잭션 충돌 문제 발생**
  - Payment 모듈의 배치작업과 Member 모듈의 배치작업을 순차적으로 진행해야 하는 상황
    - Payment 모듈의 작업 완료 이벤트를 Kafka를 통해 받으면 Member 모듈이 작업을 시작
    - 그러나 Kafka의 트랜잭션과 Spring Batch의 트랜잭션이 충돌하면서 배치작업이 실패
  - Kafka 이벤트 핸들러의 Transactional 설정을 Propagation.NOT_SUPPORTED 로 설정해 기존에 생성된 트랜잭션이 있든 없은 트랜잭션 없이 진행하도록 설정
  - Batch Job을 실행시키는 로직을 별도의 메서드로 빼고, Propagation.REQUIRES_NEW 로 설정하여 새로운 트랜잭션에서 실행하도록 설정해 문제를 해결


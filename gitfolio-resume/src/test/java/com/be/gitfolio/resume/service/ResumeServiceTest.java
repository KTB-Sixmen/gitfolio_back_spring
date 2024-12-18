package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.jwt.JWTUtil;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeResponseDTO.PaginationResponseDTO;
import com.be.gitfolio.resume.mock.*;
import com.be.gitfolio.resume.type.Template;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@Slf4j
public class ResumeServiceTest {

    private ResumeService resumeService;
    private FakeResumeRepository resumeRepository;
    private FakeLikeRepository likeRepository;
    private FakeCommentRepository commentRepository;
    private FakeAiClient aiClient;
    private FakeMemberClient memberClient;
    @Mock
    private RedisTemplate<String, String> redisTemplate;
    @Mock
    private JWTUtil jwtUtil;
    @Mock
    private S3Service s3Service;

    @BeforeEach
    void init() {
        resumeRepository = new FakeResumeRepository();
        likeRepository = new FakeLikeRepository();
        commentRepository = new FakeCommentRepository();
        aiClient = new FakeAiClient();
        memberClient = new FakeMemberClient();
        resumeService = new ResumeService(resumeRepository, likeRepository, commentRepository, redisTemplate, aiClient, memberClient, jwtUtil, s3Service);
        Resume testResume1 = Resume.builder()
                .id("testResume123") // 이력서 ID
                .memberId(1L) // 회원 ID
                .memberName("John Doe") // 회원 이름
                .avatarUrl("https://example.com/avatar.jpg") // 프로필 사진 URL
                .phoneNumber("010-1234-5678") // 전화번호
                .email("johndoe@example.com") // 이메일 주소
                .position(PositionType.BACKEND) // 포지션: 백엔드
                .techStack(List.of("Java", "Spring Boot", "MySQL")) // 기술 스택
                .aboutMe("I am a passionate backend developer.") // 자기소개
                .tags(List.of("Kakao", "Toss")) // 태그
                .workExperiences(List.of(
                        WorkExperience.builder()
                                .companyName("TechCorp")
                                .departmentName("Backend Team")
                                .role("Backend Developer")
                                .workType(WorkType.INTERN)
                                .employmentStatus(EmploymentStatus.RESIGNATION)
                                .startedAt("2021-01-01")
                                .endedAt("2023-01-01")
                                .build(),
                        WorkExperience.builder()
                                .companyName("Fintech Inc.")
                                .departmentName("Payment Gateway")
                                .role("Software Engineer")
                                .workType(WorkType.FULL_TIME)
                                .employmentStatus(EmploymentStatus.RESIGNATION)
                                .startedAt("2019-01-01")
                                .endedAt("2020-12-31")
                                .build()
                )) // 경력
                .projects(List.of(
                        Resume.Project.builder()
                                .projectName("Inventory Management System")
                                .projectStartedAt("2021-05-01")
                                .projectEndedAt("2021-12-01")
                                .skillSet("Java, Spring Boot, Hibernate")
                                .projectDescription("Developed a robust inventory management system.")
                                .repoLink("https://github.com/johndoe/inventory")
                                .build(),
                        Resume.Project.builder()
                                .projectName("Payment Gateway")
                                .projectStartedAt("2020-01-01")
                                .projectEndedAt("2020-12-31")
                                .skillSet("Node.js, MongoDB, Express")
                                .projectDescription("Integrated a scalable payment gateway.")
                                .repoLink("https://github.com/johndoe/payment")
                                .build()
                )) // 프로젝트
                .links(List.of(
                        Link.builder()
                                .linkTitle("GitHub")
                                .linkUrl("https://github.com/johndoe")
                                .build(),
                        Link.builder()
                                .linkTitle("LinkedIn")
                                .linkUrl("https://linkedin.com/in/johndoe")
                                .build()
                )) // 개인 링크
                .educations(List.of(
                        Education.builder()
                                .schoolType(SchoolType.UNIVERSITY_BACHELOR)
                                .schoolName("National University")
                                .major("Computer Science")
                                .graduationStatus(GraduationStatus.GRADUATED)
                                .startedAt("2015-03-01")
                                .endedAt("2019-02-28")
                                .build()
                )) // 학력
                .certificates(List.of(
                        Certificate.builder()
                                .certificateName("AWS Certified Solutions Architect")
                                .certificateGrade("Professional")
                                .certificatedAt("2022-06-01")
                                .certificateOrganization("Amazon Web Services")
                                .build(),
                        Certificate.builder()
                                .certificateName("Oracle Java Certification")
                                .certificateGrade("OCJP")
                                .certificatedAt("2020-09-01")
                                .certificateOrganization("Oracle")
                                .build()
                )) // 자격증
                .visibility(Visibility.PUBLIC) // 공개 여부
                .likeCount(10) // 좋아요 수
                .viewCount(100) // 조회수
                .template(Template.STAR) // 템플릿
                .build();
        ReflectionTestUtils.setField(testResume1, "createdAt", LocalDateTime.now());
        ReflectionTestUtils.setField(testResume1, "updatedAt", LocalDateTime.now());
        resumeRepository.save(testResume1);
        Resume testResume2 = Resume.builder()
                .id("testResume456") // 이력서 ID
                .memberId(2L) // 회원 ID
                .memberName("Jane Smith") // 회원 이름
                .avatarUrl("https://example.com/avatar2.jpg") // 프로필 사진 URL
                .phoneNumber("010-9876-5432") // 전화번호
                .email("janesmith@example.com") // 이메일 주소
                .position(PositionType.FRONTEND) // 포지션: 프론트엔드
                .techStack(List.of("React", "JavaScript", "CSS")) // 기술 스택
                .aboutMe("I am a creative frontend developer.") // 자기소개
                .tags(List.of("Naver", "Line")) // 태그
                .workExperiences(List.of(
                        WorkExperience.builder()
                                .companyName("CreativeTech")
                                .departmentName("Frontend Team")
                                .role("Frontend Developer")
                                .workType(WorkType.FULL_TIME)
                                .employmentStatus(EmploymentStatus.EMPLOYMENT)
                                .startedAt("2020-01-01")
                                .endedAt("2022-12-31")
                                .build()
                )) // 경력
                .projects(List.of(
                        Resume.Project.builder()
                                .projectName("E-commerce Platform")
                                .projectStartedAt("2021-01-01")
                                .projectEndedAt("2021-12-31")
                                .skillSet("React, Redux, Tailwind CSS")
                                .projectDescription("Built an e-commerce platform.")
                                .repoLink("https://github.com/janesmith/ecommerce")
                                .build()
                )) // 프로젝트
                .links(List.of(
                        Link.builder()
                                .linkTitle("GitHub")
                                .linkUrl("https://github.com/janesmith")
                                .build()
                )) // 개인 링크
                .educations(List.of(
                        Education.builder()
                                .schoolType(SchoolType.UNIVERSITY_ASSOCIATE_DEGREE)
                                .schoolName("Creative University")
                                .major("Design")
                                .graduationStatus(GraduationStatus.GRADUATED)
                                .startedAt("2016-03-01")
                                .endedAt("2018-02-28")
                                .build()
                )) // 학력
                .certificates(List.of(
                        Certificate.builder()
                                .certificateName("Certified UX Designer")
                                .certificateGrade("Advanced")
                                .certificatedAt("2019-06-01")
                                .certificateOrganization("UX Cert Organization")
                                .build()
                )) // 자격증
                .visibility(Visibility.PUBLIC) // 공개 여부
                .likeCount(20) // 좋아요 수
                .viewCount(200) // 조회수
                .template(Template.GITFOLIO) // 템플릿
                .build();

        ReflectionTestUtils.setField(testResume2, "createdAt", LocalDateTime.now());
        ReflectionTestUtils.setField(testResume2, "updatedAt", LocalDateTime.now());
        resumeRepository.save(testResume2);

        Resume testResume3 = Resume.builder()
                .id("testResume789") // 이력서 ID
                .memberId(3L) // 회원 ID
                .memberName("Emily Johnson") // 회원 이름
                .avatarUrl("https://example.com/avatar3.jpg") // 프로필 사진 URL
                .phoneNumber("010-5678-1234") // 전화번호
                .email("emilyjohnson@example.com") // 이메일 주소
                .position(PositionType.GAME)
                .techStack(List.of("Python", "TensorFlow", "Pandas")) // 기술 스택
                .aboutMe("Data is my passion.") // 자기소개
                .tags(List.of("Google", "DeepMind")) // 태그
                .workExperiences(List.of(
                        WorkExperience.builder()
                                .companyName("DataCorp")
                                .departmentName("AI Team")
                                .role("Data Scientist")
                                .workType(WorkType.FULL_TIME)
                                .employmentStatus(EmploymentStatus.EMPLOYMENT)
                                .startedAt("2019-01-01")
                                .build()
                )) // 경력
                .projects(List.of(
                        Resume.Project.builder()
                                .projectName("AI Recommendation System")
                                .projectStartedAt("2020-01-01")
                                .projectEndedAt("2022-01-01")
                                .skillSet("Python, TensorFlow, Scikit-learn")
                                .projectDescription("Developed an AI-based recommendation system.")
                                .repoLink("https://github.com/emilyjohnson/recommendation")
                                .build()
                )) // 프로젝트
                .links(List.of(
                        Link.builder()
                                .linkTitle("GitHub")
                                .linkUrl("https://github.com/emilyjohnson")
                                .build()
                )) // 개인 링크
                .educations(List.of(
                        Education.builder()
                                .schoolType(SchoolType.GRADUATE_SCHOOL_DOCTOR)
                                .schoolName("Data University")
                                .major("AI Research")
                                .graduationStatus(GraduationStatus.GRADUATED)
                                .startedAt("2012-03-01")
                                .endedAt("2018-02-28")
                                .build()
                )) // 학력
                .certificates(List.of(
                        Certificate.builder()
                                .certificateName("Certified Data Scientist")
                                .certificateGrade("Expert")
                                .certificatedAt("2020-06-01")
                                .certificateOrganization("Data Science Institute")
                                .build()
                )) // 자격증
                .visibility(Visibility.PUBLIC) // 공개 여부
                .likeCount(5) // 좋아요 수
                .viewCount(50) // 조회수
                .template(Template.STAR) // 템플릿
                .build();

        ReflectionTestUtils.setField(testResume3, "createdAt", LocalDateTime.now());
        ReflectionTestUtils.setField(testResume3, "updatedAt", LocalDateTime.now());
        resumeRepository.save(testResume3);

        likeRepository.save(Like.of("testResume123", 1L));
        commentRepository.save(Comment.builder()
                .id(1L)
                .resumeId("testResume123")
                .memberId(1L)
                .content("testContent")
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build()
        );
    }

    @Test
    void CreateResumeRequestDTO로_새로운_이력서를_생성할_수_있다() throws Exception {
        //given
        CreateResumeRequestDTO testRequest = new CreateResumeRequestDTO(
                List.of("repo1", "repo2"), // selectedRepo: 최소 하나의 레포지토리를 선택
                "프로젝트에 대한 요구사항",    // requirements
                Visibility.PUBLIC,         // visibility: 공개 여부
                Template.STAR              // template: 선택한 템플릿
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(1)
                .build());

        //when
        Resume newResume = resumeService.createResume(1L, testRequest);
        Optional<Resume> findResume = resumeRepository.findById(newResume.getId());

        //then
        assertThat(findResume.isPresent()).isTrue();
        assertThat(findResume.get().getTemplate()).isEqualTo(Template.STAR);
    }

    @Test
    void FREE플랜_사용자는_잔여_사용권이_0개_이하면_에러를_던진다() throws Exception {
        //given
        CreateResumeRequestDTO testRequest = new CreateResumeRequestDTO(
                List.of("repo1", "repo2"), // selectedRepo: 최소 하나의 레포지토리를 선택
                "프로젝트에 대한 요구사항",    // requirements
                Visibility.PUBLIC,         // visibility: 공개 여부
                Template.STAR              // template: 선택한 템플릿
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(0)
                .build());

        //when
        //then
        assertThatThrownBy(() -> {
            resumeService.createResume(1L, testRequest);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void PRO플랜_사용자는_잔여_사용권이_0개_이하여도_정상_실행된다() throws Exception {
        //given
        CreateResumeRequestDTO testRequest = new CreateResumeRequestDTO(
                List.of("repo1", "repo2"), // selectedRepo: 최소 하나의 레포지토리를 선택
                "프로젝트에 대한 요구사항",    // requirements
                Visibility.PUBLIC,         // visibility: 공개 여부
                Template.STAR              // template: 선택한 템플릿
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.PRO)
                .remainingCount(0)
                .build());

        //when
        Resume newResume = resumeService.createResume(1L, testRequest);
        Optional<Resume> findResume = resumeRepository.findById(newResume.getId());

        //then
        assertThat(findResume.isPresent()).isTrue();
        assertThat(findResume.get().getTemplate()).isEqualTo(Template.STAR);
    }

    @Test
    void UpdateResumeWithAIRequestDTO로_AI가_이력서를_수정해준_것을_확인할_수_있다() throws Exception {
        //given
        UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO = new UpdateResumeWithAIRequestDTO(
                "This is a selected text for AI processing.", // selectedText
                "Please provide suggestions to improve clarity and professionalism." // requirement
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(1)
                .build());
        aiClient.setResumeInfoForAiDTO(ResumeInfoForAiDTO.builder().email("updateEmail").build());

        //when
        ResumeInfoForAiDTO resumeInfoForAiDTO = resumeService.updateResumeWithAI(1L, "testResume123", updateResumeWithAIRequestDTO);

        //then
        assertThat(resumeInfoForAiDTO.email()).isEqualTo("updateEmail");
    }

    @Test
    void FREE플랜_사용자가_이력서_수정_시_사용권이_0개_이하면_에러를_던진다() throws Exception {
        //given
        UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO = new UpdateResumeWithAIRequestDTO(
                "This is a selected text for AI processing.", // selectedText
                "Please provide suggestions to improve clarity and professionalism." // requirement
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(0)
                .build());
        aiClient.setResumeInfoForAiDTO(ResumeInfoForAiDTO.builder().email("updateEmail").build());
        //when

        //then
        assertThatThrownBy(() -> {
            resumeService.updateResumeWithAI(1L, "testResume123", updateResumeWithAIRequestDTO);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void PRO플랜_사용자가_이력서_수정_시_사용권이_0개_이하여도_정상_실행된다() throws Exception {
        //given
        UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO = new UpdateResumeWithAIRequestDTO(
                "This is a selected text for AI processing.", // selectedText
                "Please provide suggestions to improve clarity and professionalism." // requirement
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(1)
                .build());
        aiClient.setResumeInfoForAiDTO(ResumeInfoForAiDTO.builder().email("updateEmail").build());

        //when
        ResumeInfoForAiDTO resumeInfoForAiDTO = resumeService.updateResumeWithAI(1L, "testResume123", updateResumeWithAIRequestDTO);

        //then
        assertThat(resumeInfoForAiDTO.email()).isEqualTo("updateEmail");
    }

    @Test
    void 본인의_이력서가_아닌_것을_수정하려고하면_예외를_던진다() throws Exception {
        //given
        UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO = new UpdateResumeWithAIRequestDTO(
                "This is a selected text for AI processing.", // selectedText
                "Please provide suggestions to improve clarity and professionalism." // requirement
        );
        memberClient.setMemberInfoDTO(MemberInfoDTO.builder()
                .paidPlan(PaidPlan.FREE)
                .remainingCount(1)
                .build());
        aiClient.setResumeInfoForAiDTO(ResumeInfoForAiDTO.builder().email("updateEmail").build());

        //when
        //then
        assertThatThrownBy(() -> {
            resumeService.updateResumeWithAI(99L, "testResume123", updateResumeWithAIRequestDTO);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void 포지션으로_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                PositionType.GAME, // position
                null,
                "recent", // sortOrder
                Boolean.FALSE,
                0, // page
                10 // size
        );

        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        PaginationResponseDTO<ResumeListDTO> resumeList = resumeService.getResumeList("Bearer mockJwtToken", resumeFilterDTO);

        //then
        assertThat(resumeList.content().size()).isEqualTo(1);
        assertThat(resumeList.content().get(0).position()).isEqualTo(PositionType.GAME);
    }

    @Test
    void 학력별로_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                null, // position
                SchoolType.GRADUATE_SCHOOL_DOCTOR,
                "recent", // sortOrder
                Boolean.FALSE,
                0, // page
                10 // size
        );

        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        PaginationResponseDTO<ResumeListDTO> resumeList = resumeService.getResumeList("Bearer mockJwtToken", resumeFilterDTO);

        //then
        assertThat(resumeList.content().size()).isEqualTo(1);
    }

    @Test
    void 좋아요_누른_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                null, // position
                null,
                "recent", // sortOrder
                Boolean.TRUE,
                0, // page
                10 // size
        );

        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        PaginationResponseDTO<ResumeListDTO> resumeList = resumeService.getResumeList("Bearer mockJwtToken", resumeFilterDTO);

        //then
        assertThat(resumeList.content().size()).isEqualTo(1);
        assertThat(resumeList.content().get(0).resumeId()).isEqualTo("testResume123");
    }

    @Test
    void 조회수_순으로_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                null, // position
                null,
                "view", // sortOrder
                Boolean.FALSE,
                0, // page
                10 // size
        );

        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        PaginationResponseDTO<ResumeListDTO> resumeList = resumeService.getResumeList("Bearer mockJwtToken", resumeFilterDTO);

        //then
        assertThat(resumeList.content().size()).isEqualTo(3);
        assertThat(resumeList.content().get(0).viewCount()).isEqualTo(200);
    }

    @Test
    void 좋아요_순으로_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                null, // position
                null,
                "like", // sortOrder
                Boolean.FALSE,
                0, // page
                10 // size
        );

        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        PaginationResponseDTO<ResumeListDTO> resumeList = resumeService.getResumeList("Bearer mockJwtToken", resumeFilterDTO);

        //then
        assertThat(resumeList.content().size()).isEqualTo(3);
        assertThat(resumeList.content().get(0).likeCount()).isEqualTo(20);
    }

    @Test
    void 커뮤니티에서_이력서_상세정보를_조회할_수_있고_조회수를_증가시킬_수_있다() throws Exception {
        //given
        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);
        Mockito.when(redisTemplate.hasKey(any())).thenReturn(false); // Redis에 키가 없는 상황
        Mockito.when(redisTemplate.opsForValue()).thenReturn(mock(ValueOperations.class));

        //when
        ResumeDetailDTO communityResumeDetail = resumeService.getCommunityResumeDetail("Bearer mockJwtToken", "testResume123", "mockClientIp");

        //then
        assertThat(communityResumeDetail.projects().get(0).getProjectName()).isEqualTo("Inventory Management System");
        assertThat(communityResumeDetail.viewCount()).isEqualTo(101);
    }

    @Test
    void 내_이력서_상세조회를_할_수_있다() throws Exception {
        //given
        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);
        Mockito.when(redisTemplate.hasKey(any())).thenReturn(false); // Redis에 키가 없는 상황
        Mockito.when(redisTemplate.opsForValue()).thenReturn(mock(ValueOperations.class));

        //when
        ResumeDetailDTO myResumeDetail = resumeService.getMyResumeDetail("Bearer mockJwtToken", "testResume123", "mockClientIp");

        //then
        assertThat(myResumeDetail.resumeId()).isEqualTo("testResume123");

    }

    @Test
    void 내_이력서_API로_다른사람의_이력서를_상세조회하면_예외를_던진다() throws Exception {
        //given
        Mockito.when(jwtUtil.getMemberId(any())).thenReturn(1L);

        //when
        //then
        assertThatThrownBy(() -> {
            resumeService.getMyResumeDetail("Bearer mockJwtToken", "testResume456", "mockClientIp");
        }).isInstanceOf(BaseException.class);

    }

    @Test
    void 내_이력서_목록을_조회할_수_있다() throws Exception {
        //given
        //when
        PaginationResponseDTO<ResumeListDTO> myResumeList = resumeService.getMyResumeList(1L, 0, 10);

        //then
        assertThat(myResumeList.content().size()).isEqualTo(1);
        assertThat(myResumeList.content().get(0).resumeId()).isEqualTo("testResume123");

    }

    @Test
    void 이력서를_삭제할_수_있고_해당_이력서_관련_댓글과_좋아요_모두_삭제된다() throws Exception {
        //given
        //when
        resumeService.deleteResume("testResume123");

        //then
        Optional<Resume> resume = resumeRepository.findById("testResume");
        List<Like> likes = likeRepository.findByResumeId("testResume123");
        List<Comment> comments = commentRepository.findAllByResumeId("testResume123");
        assertThat(resume.isEmpty()).isTrue();
        assertThat(likes.size()).isEqualTo(0);
        assertThat(comments.size()).isEqualTo(0);

    }

    @Test
    void UpdateResumeRequestDTO로_이력서를_직접_수정할_수_있다() throws Exception {
        //given
        UpdateResumeRequestDTO updateResumeRequestDTO = new UpdateResumeRequestDTO(
                List.of(), "updatedAboutMe", List.of(), List.of(), List.of(), List.of(), List.of(), List.of()
        );
        MockMultipartFile imageFile = new MockMultipartFile("image", "avatar.png", "image/png", "dummy data".getBytes());
        Mockito.when(s3Service.uploadFile(any())).thenReturn("updatedAvatarUrl");

        //when
        resumeService.updateResume(1L, "testResume123", updateResumeRequestDTO, imageFile, isAiFixed);

        //then
        Optional<Resume> resume = resumeRepository.findById("testResume123");
        assertThat(resume.get().getAboutMe()).isEqualTo("updatedAboutMe");
        assertThat(resume.get().getAvatarUrl()).isEqualTo("updatedAvatarUrl");

    }

    @Test
    void 내_이력서가_아닌데_수정하려고_하면_예외를_던진다() throws Exception {
        //given
        UpdateResumeRequestDTO updateResumeRequestDTO = new UpdateResumeRequestDTO(
                List.of(), "updatedAboutMe", List.of(), List.of(), List.of(), List.of(), List.of(), List.of()
        );
        MockMultipartFile imageFile = new MockMultipartFile("image", "avatar.png", "image/png", "dummy data".getBytes());

        //when
        //then
        assertThatThrownBy(() -> {
            resumeService.updateResume(2L, "testResume123", updateResumeRequestDTO, imageFile, isAiFixed);
        }).isInstanceOf(BaseException.class);

    }

    @Test
    void UpdateVisibilityDTO로_이력서_공개여부를_수정할_수_있다() throws Exception {
        //given
        UpdateVisibilityDTO updateVisibilityDTO = new UpdateVisibilityDTO(Visibility.PRIVATE);

        //when
        resumeService.updateVisibility(1L, "testResume123", updateVisibilityDTO);

        //then
        Optional<Resume> resume = resumeRepository.findById("testResume123");
        assertThat(resume.get().getVisibility()).isEqualTo(Visibility.PRIVATE);

    }
}

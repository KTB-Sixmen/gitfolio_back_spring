package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.dto.*;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.mock.FakeCommentRepository;
import com.be.gitfolio.resume.mock.FakeMemberClient;
import com.be.gitfolio.resume.mock.FakeResumeRepository;
import com.be.gitfolio.resume.type.Template;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;

@ExtendWith(MockitoExtension.class)
public class CommentServiceTest {

    private CommentService commentService;
    private ResumeEventPublisher resumeEventPublisher;
    private FakeCommentRepository commentRepository;
    private FakeResumeRepository resumeRepository;
    private FakeMemberClient memberClient;

    @Mock
    private S3Service s3Service;

    @BeforeEach
    void init() {
        resumeRepository = new FakeResumeRepository();
        commentRepository = new FakeCommentRepository();
        memberClient = new FakeMemberClient();
        resumeEventPublisher = Mockito.mock(ResumeEventPublisher.class);
        commentService = new CommentService(resumeRepository, commentRepository, resumeEventPublisher, s3Service, memberClient);
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
                                .roleAndTask(List.of(
                                        "Designed and implemented a scalable inventory management system.",
                                        "Developed real-time stock tracking features.",
                                        "Optimized database performance with Hibernate."
                                ))
                                .star(Resume.Star.builder()
                                        .situation("The client needed a centralized system to track and manage inventory across multiple warehouses.")
                                        .task("Develop a system that provides real-time stock updates and supports concurrent users.")
                                        .action("Built a Spring Boot application with Hibernate for database management and integrated RESTful APIs for user access.")
                                        .result("Enabled real-time inventory tracking, reducing stock discrepancies by 30%.")
                                        .build())
                                .troubleShooting(Resume.TroubleShooting.builder()
                                        .problem("Concurrency issues during stock updates caused inconsistent inventory data.")
                                        .hypothesis("The issue was due to improper transaction management in high-concurrency environments.")
                                        .tring("Implemented proper transaction isolation levels and optimized Hibernate session handling.")
                                        .result("Resolved data inconsistency and improved transaction reliability under high user load.")
                                        .build())
                                .repoLink("https://github.com/johndoe/inventory")
                                .build(),

                        Resume.Project.builder()
                                .projectName("Payment Gateway")
                                .projectStartedAt("2020-01-01")
                                .projectEndedAt("2020-12-31")
                                .skillSet("Node.js, MongoDB, Express")
                                .roleAndTask(List.of(
                                        "Integrated a scalable payment gateway for secure transactions.",
                                        "Developed fraud detection mechanisms.",
                                        "Optimized API response times for high-traffic scenarios."
                                ))
                                .star(Resume.Star.builder()
                                        .situation("The client required a secure and scalable payment system for their e-commerce platform.")
                                        .task("Integrate a payment gateway capable of handling high transaction volumes and ensuring security.")
                                        .action("Developed the gateway using Node.js and MongoDB, implemented tokenized payment processing, and ensured API security.")
                                        .result("Handled 20,000 transactions daily with a 99.9% success rate.")
                                        .build())
                                .troubleShooting(Resume.TroubleShooting.builder()
                                        .problem("High API response time during peak traffic.")
                                        .hypothesis("The latency was caused by inefficient database queries and redundant API calls.")
                                        .tring("Optimized MongoDB queries with indexing and reduced API call redundancy.")
                                        .result("Improved API response time by 40%, ensuring smooth transaction processing during peak hours.")
                                        .build())
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
    void createComment로_댓글을_생성할_수_있다() throws Exception {
        //given
        CommentRequestDTO commentRequestDTO = new CommentRequestDTO("TestCommentRequestContent");
        Mockito.doNothing().when(resumeEventPublisher).publishResumeEvent(anyLong(), anyString(), anyLong(), anyString(), any(NotificationType.class));

        //when
        Comment comment = commentService.createComment("testResume123", 1L, "testSender", commentRequestDTO);

        //then
        assertThat(comment.getContent()).isEqualTo("TestCommentRequestContent");
    }

    @Test
    void 존재하지_않는_이력서에_댓글을_생성하려고_하면_에러를_던진다() throws Exception {
        //given
        CommentRequestDTO commentRequestDTO = new CommentRequestDTO("TestCommentRequestContent");

        //when
        //then
        assertThatThrownBy(() -> {
            commentService.createComment("NotExistResume", 1L, "testSender", commentRequestDTO);
        }).isInstanceOf(BaseException.class);

    }

    @Test
    void updateComment로_댓글을_수정할_수_있다() throws Exception {
        //given
        Optional<Comment> comment = commentRepository.findById(1L);
        CommentRequestDTO commentRequestDTO = new CommentRequestDTO("UpdatedContent");

        //when
        commentService.updateComment(1L, 1L, commentRequestDTO);

        //then
        assertThat(comment.get().getContent()).isEqualTo("UpdatedContent");
    }

    @Test
    void 댓글_수정_시_다른_사람의_댓글을_수정하려하면_에러를_던진다() throws Exception {
        //given
        CommentRequestDTO commentRequestDTO = new CommentRequestDTO("UpdatedContent");

        //when
        //then
        assertThatThrownBy(() -> {
            commentService.updateComment(1L, 2L, commentRequestDTO);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void 존재하지_않는_댓글을_수정하려고_하면_에러를_던진다() throws Exception {
        //given
        CommentRequestDTO commentRequestDTO = new CommentRequestDTO("UpdatedContent");

        //when
        //then
        assertThatThrownBy(() -> {
            commentService.updateComment(99L, 2L, commentRequestDTO);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void deleteComment로_댓글을_삭제할_수_있다() throws Exception {
        //given

        //when
        commentService.deleteComment(1L, 1L);
        Optional<Comment> comment = commentRepository.findById(1L);

        //then
        assertThat(comment.isEmpty()).isTrue();
    }

    @Test
    void 존재하지_않는_댓글을_삭제하려고_하면_에러를_던진다() throws Exception {
        //given
        //when
        //then
        assertThatThrownBy(() -> {
            commentService.deleteComment(99L, 2L);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void 이력서별_댓글을_조회할_수_있다() throws Exception {
        //given
        memberClient.setMemberInfoDTO(ResumeRequestDTO.MemberInfoDTO.builder().avatarUrl("testAvatarUrl").build());

        //when
        List<CommentResponseDTO> commentList = commentService.getCommentList("testResume123");


        //then
        assertThat(commentList.get(0).content()).isEqualTo("testContent");
    }
}

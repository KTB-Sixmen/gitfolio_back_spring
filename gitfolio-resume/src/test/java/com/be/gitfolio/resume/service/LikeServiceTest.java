package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.mock.FakeLikeRepository;
import com.be.gitfolio.resume.mock.FakeMemberClient;
import com.be.gitfolio.resume.mock.FakeResumeRepository;
import com.be.gitfolio.resume.type.Template;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.BDDMockito;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.mongodb.core.MongoTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;

@ExtendWith(MockitoExtension.class)
public class LikeServiceTest {

    private LikeService likeService;
    private FakeResumeRepository resumeRepository;
    private FakeLikeRepository likeRepository;
    private FakeMemberClient memberClient;
    @Mock
    private ResumeEventPublisher resumeEventPublisher;
    @Mock
    private MongoTemplate mongoTemplate;

    @BeforeEach
    void init() {
        resumeRepository = new FakeResumeRepository();
        likeRepository = new FakeLikeRepository();
        memberClient = new FakeMemberClient();
        likeService = new LikeService(resumeRepository, likeRepository, mongoTemplate, resumeEventPublisher, memberClient);
        resumeRepository.save(Resume.builder()
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
                .build()
        );
        likeRepository.save(Like.of("testResume123", 1L));
    }

    @Test
    void 좋아요_누르지_않은_상태에서_좋아요를_누르면_좋아요가_생성된다() throws Exception {
        //given
        Resume mockedResume = Resume.builder()
                .id("testestest111")
                .likeCount(0)
                .build();
        memberClient.setMemberInfoDTO(ResumeRequestDTO.MemberInfoDTO.builder().nickname("testNickname").build());
        Mockito.when(mongoTemplate.findAndModify(any(), any(), any())).thenReturn(mockedResume);

        //when
        boolean isCreated = likeService.toggleLike("testResume123", 2L);

        //then
        Optional<Like> like = likeRepository.findByResumeIdAndMemberId("testResume123", 2L);
        assertThat(isCreated).isTrue();
        assertThat(like.isPresent()).isTrue();
    }

    @Test
    void 좋아요_누른_상태에서_좋아요를_누르면_좋아요가_삭제된다() throws Exception {
        //given
        Resume mockedResume = Resume.builder()
                .id("testestest111")
                .likeCount(0)
                .build();
        Mockito.when(mongoTemplate.findAndModify(any(), any(), any())).thenReturn(mockedResume);

        //when
        boolean isCreated = likeService.toggleLike("testResume123", 1L);

        //then
        Optional<Like> like = likeRepository.findByResumeIdAndMemberId("testResume123", 1L);
        assertThat(isCreated).isFalse();
        assertThat(like.isEmpty()).isTrue();
    }
}

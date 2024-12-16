package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMongo;
import com.be.gitfolio.common.dto.*;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.type.Template;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;
import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Document(collection = "resume")
public class Resume {

    @Id
    private String id;  // 이력서 ID
    private Long memberId;  // 회원 ID
    private String memberName; // 회원 이름
    private String avatarUrl; // 프로필 사진
    private String phoneNumber; // 전화번호
    private String email; // 이메일 주소
    private PositionType position; // 포지션(직군)
    private List<String> techStack;  // 기술 스택
    private String aboutMe;  // 자기소개
    private List<String> tags;  // 태그 (회사명 등)
    private List<WorkExperience> workExperiences;  // 경력
    private List<Project> projects;  // 프로젝트
    private List<Link> links;  // 개인 링크
    private List<Education> educations;  // 학력
    private List<Certificate> certificates;  // 자격증
    @Enumerated(EnumType.STRING)
    private Visibility visibility; // 공개 여부
    private int likeCount;  // 좋아요 수
    private int viewCount;  // 조회수
    @Enumerated(EnumType.STRING)
    private Template template;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public void validateVisibility() {
        if (this.getVisibility().equals(Visibility.PRIVATE)) {
            throw new BaseException(ErrorCode.RESUME_ACCESS_DENIED);
        }
    }

    public void validateOwnerShip(Long memberId) {
        if (!this.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_ACCESS_RESUME);
        }
    }

    public void updateVisibility(Visibility visibility) {
        this.visibility = visibility;
        this.updatedAt = LocalDateTime.now();
    }

    public void increaseView() {
        this.viewCount++;
    }

    public void updateResume(UpdateResumeRequestDTO updateResumeDTO, String avatarUrl) {
        this.avatarUrl = avatarUrl;
        this.techStack = updateResumeDTO.techStack();
        this.aboutMe = updateResumeDTO.aboutMe();
        this.tags = updateResumeDTO.tags();
        this.workExperiences = updateResumeDTO.workExperiences();
        this.projects = updateResumeDTO.projects();
        this.links = updateResumeDTO.links();
        this.educations = updateResumeDTO.educations();
        this.certificates = updateResumeDTO.certificates();
        this.updatedAt = LocalDateTime.now();
    }

    public static Resume of(MemberInfoDTO memberInfoDTO, AIResponseDTO aiResponseDTO, CreateResumeRequestDTO createResumeRequestDTO) {
        return Resume.builder()
                .memberId(memberInfoDTO.memberId())
                .memberName(memberInfoDTO.name())
                .avatarUrl(memberInfoDTO.avatarUrl())
                .phoneNumber(memberInfoDTO.phoneNumber())
                .email(memberInfoDTO.email())
                .position(memberInfoDTO.position())
                .techStack(aiResponseDTO.techStack())
                .aboutMe(aiResponseDTO.aboutMe())
                .workExperiences(memberInfoDTO.workExperiences())
                .projects(aiResponseDTO.projects())
                .links(memberInfoDTO.links())
                .educations(memberInfoDTO.educations())
                .certificates(memberInfoDTO.certificates())
                .visibility(createResumeRequestDTO.visibility())
                .likeCount(0)
                .viewCount(0)
                .template(createResumeRequestDTO.template())
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor(access = AccessLevel.PROTECTED)
    public static class Project {
        private String projectName;
        private String projectStartedAt;
        private String projectEndedAt;
        private String skillSet;
        private String projectDescription;
        private String repoLink;
    }
}

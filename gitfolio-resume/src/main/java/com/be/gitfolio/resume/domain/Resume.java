package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMongo;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "resume")
public class Resume extends BaseEntityMongo {

    @Id
    private String id;  // 이력서 ID
    private String memberId;  // 회원 ID
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
    private int likeCount;  // 좋아요 수
    private int viewCount;  // 조회수

    public void updateView() {
        this.viewCount++;
    }

    public void increaseLike() {
        this.likeCount++;
    }

    public void decreaseLike() {
        this.likeCount--;
    }
    public void updateResume(UpdateResumeRequestDTO updateResumeDTO) {
        this.techStack = updateResumeDTO.techStack();
        this.aboutMe = updateResumeDTO.aboutMe();
        this.tags = updateResumeDTO.tags();
        this.workExperiences = updateResumeDTO.workExperiences();
        this.projects = updateResumeDTO.projects();
        this.links = updateResumeDTO.links();
        this.educations = updateResumeDTO.educations();
        this.certificates = updateResumeDTO.certificates();
    }

    public static Resume of(MemberInfoDTO memberInfoDTO, AIResponseDTO aiResponseDTO) {
        return Resume.builder()
                .memberId(memberInfoDTO.memberId())
                .memberName(memberInfoDTO.memberName())
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
                .likeCount(0)
                .viewCount(0)
                .build();
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Project {
        private String projectName;
        private String projectStartedAt;
        private String projectEndedAt;
        private String skillSet;
        private String projectDescription;
        private String repoLink;
    }

        @Getter
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class WorkExperience {
            private String companyName;
            private String departmentName;
            private String role;
            private WorkType workType;
            private EmploymentStatus employmentStatus;
            private String startedAt;
            private String endedAt;
        }

        @Getter
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class Education {
            private SchoolType schoolType;
            private String schoolName;
            private String major;
            private GraduationStatus graduationStatus;
            private String startedAt;
            private String endedAt;
        }

        @Getter
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class Certificate {
            private String certificateName;
            private String certificateGrade;
            private String certificatedAt;
            private String certificateOrganization;
        }

        @Getter
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class Link {
            private String linkTitle;
            private String linkUrl;
        }


    }

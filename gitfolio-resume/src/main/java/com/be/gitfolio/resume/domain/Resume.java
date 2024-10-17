package com.be.gitfolio.resume.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "resume")
public class Resume {

    @Id
    private String id;  // 이력서 ID
    private String memberId;  // 회원 ID
    private List<String> techStack;  // 기술 스택
    private String aboutMe;  // 자기소개
    private List<String> tags;  // 태그 (회사명 등)
    private List<WorkExperience> workExperiences;  // 경력
    private List<Project> projects;  // 프로젝트
    private List<Link> links;  // 개인 링크
    private List<Education> educations;  // 학력
    private List<Activity> activities;  // 대외 활동
    private List<Certificate> certificates;  // 자격증
    private int likeCount;  // 좋아요 수
    private int viewCount;  // 조회수

    public void updateView() {
        this.viewCount++;
    }
    public void updateResume(UpdateResumeRequestDTO updateResumeDTO) {
        this.techStack = updateResumeDTO.getTechStack();
        this.aboutMe = updateResumeDTO.getAboutMe();
        this.tags = updateResumeDTO.getTags();
        this.workExperiences = updateResumeDTO.getWorkExperiences();
        this.projects = updateResumeDTO.getProjects();
        this.links = updateResumeDTO.getLinks();
        this.educations = updateResumeDTO.getEducations();
        this.activities = updateResumeDTO.getActivities();
        this.certificates = updateResumeDTO.getCertificates();
    }

    public static Resume of(String memberId, CreateResumeRequestDTO createResumeRequestDTO) {
        return Resume.builder()
                .memberId(memberId)
                .techStack(createResumeRequestDTO.getTechStack())
                .aboutMe(createResumeRequestDTO.getAboutMe())
                .tags(createResumeRequestDTO.getTags())
                .workExperiences(createResumeRequestDTO.getWorkExperiences())
                .projects(createResumeRequestDTO.getProjects())
                .links(createResumeRequestDTO.getLinks())
                .educations(createResumeRequestDTO.getEducations())
                .activities(createResumeRequestDTO.getActivities())
                .certificates(createResumeRequestDTO.getCertificates())
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
            private String workTime;
            private String employmentStatus;
            private String workStartedAt;
            private String workEndedAt;
        }

        @Getter
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class Education {
            private String schoolType;
            private String schoolName;
            private String major;
            private String graduationStatus;
            private String enrollmentStartAt;
            private String enrollmentEndedAt;
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
        public static class Activity {
            private String activityName;
            private int activityYear;
            private String activityDescription;
            private String activityOrganization;
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

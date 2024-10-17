package com.be.gitfolio.resume.dto;

import com.be.gitfolio.resume.domain.Resume;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

public class ResumeResponseDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ResumeListDTO {
        private String resumeId;
        private Long memberId;
        private String aboutMe;
        private List<String> tags;
        private int likeCount;
        private int viewCount;

        public ResumeListDTO(Resume resume) {
            this.resumeId = resume.getId();
            this.memberId = Long.parseLong(resume.getMemberId());
            this.aboutMe = resume.getAboutMe();
            this.tags = resume.getTags();
            this.likeCount = resume.getLikeCount();
            this.viewCount = resume.getViewCount();
        }
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ResumeDetailDTO {
        private String resumeId;
        private Long memberId;
        private List<String> techStack;  // 기술 스택
        private String aboutMe;  // 자기소개
        private List<String> tags;  // 태그 (회사명 등)
        private List<Resume.WorkExperience> workExperiences;  // 경력
        private List<Resume.Project> projects;  // 프로젝트
        private List<Resume.Link> links;  // 개인 링크
        private List<Resume.Education> educations;  // 학력
        private List<Resume.Activity> activities;  // 대외 활동
        private List<Resume.Certificate> certificates;  // 자격증
        private int likeCount;  // 좋아요 수
        private int viewCount;  // 조회수

        public ResumeDetailDTO(Resume resume) {
            this.resumeId = resume.getId();
            this.memberId = Long.parseLong(resume.getMemberId());
            this.techStack = resume.getTechStack();
            this.aboutMe = resume.getAboutMe();
            this.tags = resume.getTags();
            this.workExperiences = resume.getWorkExperiences();
            this.projects = resume.getProjects();
            this.links = resume.getLinks();
            this.educations = resume.getEducations();
            this.activities = resume.getActivities();
            this.certificates = resume.getCertificates();
            this.likeCount = resume.getLikeCount();
            this.viewCount = resume.getViewCount();
        }
    }

    // TODO: AI 쪽 만들어지면 사용해야함!
    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class AIResponseDTO {
        private List<Resume.Project> projects;  // 프로젝트
        private List<String> tags;
        private String aboutMe;
    }
}

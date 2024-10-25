package com.be.gitfolio.resume.dto;

import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Page;

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
    public static class PaginationResponseDTO<T> {
        private int currentPage;
        private int totalPages;
        private long totalElements;
        private int size;
        private List<T> content;

        public PaginationResponseDTO(Page<T> page) {
            this.currentPage = page.getNumber();
            this.totalPages = page.getTotalPages();
            this.totalElements = page.getTotalElements();
            this.size = page.getSize();
            this.content = page.getContent();
        }
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ResumeDetailDTO {
        private String resumeId;
        private Long memberId;  // 회원 ID
        private String memberName; // 회원 이름
        private String avatarUrl; // 프로필 사진
        private String phoneNumber; // 전화번호
        private String email; // 이메일 주소
        private String position; // 포지션(직군)
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
            this.memberId = Long.valueOf(resume.getMemberId());
            this.memberName = resume.getMemberName();
            this.avatarUrl = resume.getAvatarUrl();
            this.phoneNumber = resume.getPhoneNumber();
            this.email = resume.getEmail();
            this.position = resume.getPosition();
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
        private List<String> techStack;
        private String aboutMe;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CommentResponseDTO {
        private Long id;
        private String resumeId;
        private Long memberId;
        private String content;

        public CommentResponseDTO(Comment comment) {
            this.id = comment.getId();
            this.resumeId = comment.getResumeId();
            this.memberId = comment.getMemberId();
            this.content = comment.getContent();
        }
    }

}

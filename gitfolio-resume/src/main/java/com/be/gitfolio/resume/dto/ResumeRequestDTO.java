package com.be.gitfolio.resume.dto;

import com.be.gitfolio.resume.domain.Resume;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

public class ResumeRequestDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CreateResumeRequestDTO {
        private List<String> techStack;  // 기술 스택
        private String aboutMe;  // 자기소개
        private List<String> tags;  // 태그 (회사명 등)
        private List<Resume.WorkExperience> workExperiences;  // 경력
        private List<Resume.Project> projects;  // 프로젝트
        private List<Resume.Link> links;  // 개인 링크
        private List<Resume.Education> educations;  // 학력
        private List<Resume.Activity> activities;  // 대외 활동
        private List<Resume.Certificate> certificates;  // 자격증

        // TODO: AI 모듈 만들어지면 위에꺼 지우고 아래꺼로 써야함
//        private List<String> selectedRepo; // 선택한 레포
//        private String requirements; // 요구사항(강조사항)
    }

    // TODO: AI 쪽 만들어지면 사용해야함!
    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class AIRequestDTO {
        private String githubID;    // 회원 깃허브 아이디(nickname 필드)
        private String personalRepo;    // 개인레포(깃허브 자기 소개페이지?)
        private List<String> selectedRepo;  // 이력서 생성에 사용할 레포 목록
        private String requirements;    // 요구사항(강조사항)
    }



    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class UpdateResumeRequestDTO {
        private List<String> techStack;  // 기술 스택
        private String aboutMe;  // 자기소개
        private List<String> tags;  // 태그 (회사명 등)
        private List<Resume.WorkExperience> workExperiences;  // 경력
        private List<Resume.Project> projects;  // 프로젝트
        private List<Resume.Link> links;  // 개인 링크
        private List<Resume.Education> educations;  // 학력
        private List<Resume.Activity> activities;  // 대외 활동
        private List<Resume.Certificate> certificates;  // 자격증
    }
}

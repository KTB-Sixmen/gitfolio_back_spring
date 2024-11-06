package com.be.gitfolio.resume.dto;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.resume.domain.Resume;
import jakarta.validation.constraints.*;
import lombok.*;

import java.util.List;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

public class ResumeRequestDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    @ToString
    public static class MemberInfoDTO {
        @NotBlank
        private String memberId;  // 회원 ID
        @NotBlank
        private String memberName;
        @NotBlank
        private String avatarUrl;
        @Pattern(regexp = "^\\d{3}\\d{3,4}\\d{4}$", message = "전화번호 형식이 올바르지 않습니다. 예시: 01012345678")
        private String phoneNumber;
        @Email(message = "올바른 이메일 형식이어야 합니다.")
        private String email;
        @NotBlank
        private PositionType position;
        private List<Resume.WorkExperience> workExperiences;  // 경력
        private List<Resume.Link> links;  // 개인 링크
        private List<Resume.Education> educations;  // 학력
        private List<Resume.Certificate> certificates;  // 자격증
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CreateResumeRequestDTO {
        @NotEmpty(message = "적어도 하나의 레포지토리를 선택해야 합니다.")
        private List<String> selectedRepo;
        private String requirements;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class AIRequestDTO {
        private String githubID;    // 회원 깃허브 아이디(nickname 필드)
        private String githubName;  // 회원 깃허브 이름(설정 안하면 null)
        private String personalRepo;    // 개인레포(깃허브 자기 소개페이지?)
        private List<String> selectedRepo;  // 이력서 생성에 사용할 레포 목록
        private String requirements;    // 요구사항(강조사항)

        public static AIRequestDTO of(MemberResponseById memberResponse, String personalRepo, CreateResumeRequestDTO createResumeRequestDTO) {
            return AIRequestDTO.builder()
                    .githubID(memberResponse.getNickname())
                    .githubName(memberResponse.getGithubName())
                    .personalRepo(personalRepo)
                    .selectedRepo(createResumeRequestDTO.getSelectedRepo())
                    .requirements(createResumeRequestDTO.getRequirements())
                    .build();
        }
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
        private List<Resume.Certificate> certificates;  // 자격증
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class CommentRequestDTO {
        @NotBlank(message = "내용은 비어있을 수 없습니다.")
        private String content;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ResumeFilterDTO {
        private String tag; // 회사별
        private String position; // 직군별
        private String techStack; // 기술 스택별
        private String schoolType; // 학력별
        private String sortOrder; // 정렬기준

        @Min(0)
        private int page = 0;
        @Min(0)
        private int size = 10; // TODO: 한페이지에 몇개씩 조회할건지 픽스해야함
    }

}

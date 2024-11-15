package com.be.gitfolio.resume.dto;

import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.common.type.Visibility;
import com.be.gitfolio.resume.domain.Resume;
import jakarta.validation.constraints.*;

import java.util.List;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

public class ResumeRequestDTO {

    public record MemberInfoDTO(
            @NotBlank Long memberId,
            String memberAdditionalInfoId,
            String nickname,
            @NotBlank String name,
            String username,
            String githubName,
            @NotBlank String avatarUrl,
            @Pattern(regexp = "^\\d{3}\\d{3,4}\\d{4}$", message = "전화번호 형식이 올바르지 않습니다. 예시: 01012345678") String phoneNumber,
            @Email(message = "올바른 이메일 형식이어야 합니다.") String email,
            @NotBlank PositionType position,
            List<Resume.WorkExperience> workExperiences,
            List<Resume.Education> educations,
            List<Resume.Certificate> certificates,
            List<Resume.Link> links
    ) {}

    public record CreateResumeRequestDTO(
            @NotEmpty(message = "적어도 하나의 레포지토리를 선택해야 합니다.") List<String> selectedRepo,
            String requirements,
            @NotNull(message = "공개 여부는 필수 항목입니다.") Visibility visibility
    ) {}

    public record AIRequestDTO(
            String githubID,    // 회원 깃허브 아이디(nickname 필드)
            String githubName,  // 회원 깃허브 이름(설정 안하면 null)
            String personalRepo,    // 개인레포(깃허브 자기 소개페이지?)
            List<String> selectedRepo,  // 이력서 생성에 사용할 레포 목록
            String requirements    // 요구사항(강조사항)
    ) {
        public static AIRequestDTO of(MemberInfoDTO memberInfoDTO, String personalRepo, CreateResumeRequestDTO createResumeRequestDTO) {
            return new AIRequestDTO(
                    memberInfoDTO.nickname(),
                    memberInfoDTO.githubName(),
                    personalRepo,
                    createResumeRequestDTO.selectedRepo(),
                    createResumeRequestDTO.requirements()
            );
        }
    }

    public record UpdateResumeRequestDTO(
            List<String> techStack,  // 기술 스택
            String aboutMe,  // 자기소개
            List<String> tags,  // 태그 (회사명 등)
            List<Resume.WorkExperience> workExperiences,  // 경력
            List<Resume.Project> projects,  // 프로젝트
            List<Resume.Link> links,  // 개인 링크
            List<Resume.Education> educations,  // 학력
            List<Resume.Certificate> certificates  // 자격증
    ) {}

    public record CommentRequestDTO(
            @NotBlank(message = "내용은 비어있을 수 없습니다.") String content
    ) {}

    public record ResumeFilterDTO(
            String tag, // 회사별
            String position, // 직군별
            String techStack, // 기술 스택별
            String schoolType, // 학력별
            String sortOrder, // 정렬기준
            Boolean liked, // 좋아요 필터링
            @Min(0) int page,
            @Min(0) int size
    ) {
        public ResumeFilterDTO {
            // 기본값을 설정
            if (page < 0) page = 0;
            if (size <= 0) size = 10;
        }
    }
}

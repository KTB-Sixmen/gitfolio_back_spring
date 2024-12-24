package com.be.gitfolio.resume.dto;

import com.be.gitfolio.common.dto.*;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.common.type.SchoolType;
import com.be.gitfolio.common.type.Visibility;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.type.Template;
import jakarta.validation.constraints.*;
import lombok.Builder;

import java.util.List;

import static com.be.gitfolio.resume.domain.Resume.*;

public class ResumeRequestDTO {

    @Builder
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
            PaidPlan paidPlan,
            Integer remainingCount,
            List<WorkExperience> workExperiences,
            List<Education> educations,
            List<Certificate> certificates,
            List<Link> links
    ) {
        public void validateRemainingCount() {
            if (this.paidPlan().equals(PaidPlan.FREE) && this.remainingCount() <= 0) {
                throw new BaseException(ErrorCode.REMAINING_COUNT_EXCEEDED);
            }
        }
    }

    public record CreateResumeRequestDTO(
            @NotEmpty(message = "적어도 하나의 레포지토리를 선택해야 합니다.") List<String> selectedRepo,
            String requirements,
            @NotNull(message = "공개 여부는 필수 항목입니다.") Visibility visibility,
            @NotNull(message = "템플릿 선택은 필수 항목입니다.") Template template
    ) {}

    public record UpdateResumeWithAIRequestDTO(
            String selectedText,
            @NotEmpty(message = "요구사항은 필수 항목입니다.") String requirement
    ) {}

    @Builder
    public record ResumeInfoForAiDTO(
            Template template,
            String resumeId,
            Long memberId,
            String memberName,
            String avatarUrl,
            String email,
            PositionType position,
            List<String> techStack,
            String aboutMe,
            List<String> tags,
            List<WorkExperience> workExperiences,
            List<Project> projects,
            List<Link> links,
            List<Education> educations,
            List<Certificate> certificates
    ) {
        public static ResumeInfoForAiDTO from(Resume resume, String avatarUrl) {
            return new ResumeInfoForAiDTO(
                    resume.getTemplate(),
                    resume.getId(),
                    resume.getMemberId(),
                    resume.getMemberName(),
                    avatarUrl,
                    resume.getEmail(),
                    resume.getPosition(),
                    resume.getTechStack(),
                    resume.getAboutMe(),
                    resume.getTags(),
                    resume.getWorkExperiences(),
                    resume.getProjects(),
                    resume.getLinks(),
                    resume.getEducations(),
                    resume.getCertificates()
            );
        }
    }

    public record AIUpdateRequestDTO(
        String selectedText,
        String requirement,
        ResumeInfoForAiDTO resumeInfo
    ) {
        public static AIUpdateRequestDTO of(UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO,
                                            ResumeInfoForAiDTO resumeInfo) {
            return new AIUpdateRequestDTO(
                    updateResumeWithAIRequestDTO.selectedText(),
                    updateResumeWithAIRequestDTO.requirement(),
                    resumeInfo
            );
        }
    }

    public record AIRequestDTO(
            String githubID,    // 회원 깃허브 아이디(nickname 필드)
            String githubName,  // 회원 깃허브 이름(설정 안하면 null)
            String personalRepo,    // 개인레포(깃허브 자기 소개페이지?)
            List<String> selectedRepo,  // 이력서 생성에 사용할 레포 목록
            String requirements,    // 요구사항(강조사항)
            Template template
    ) {
        public static AIRequestDTO of(MemberInfoDTO memberInfoDTO, String personalRepo, CreateResumeRequestDTO createResumeRequestDTO) {
            return new AIRequestDTO(
                    memberInfoDTO.nickname(),
                    memberInfoDTO.githubName(),
                    personalRepo,
                    createResumeRequestDTO.selectedRepo(),
                    createResumeRequestDTO.requirements(),
                    createResumeRequestDTO.template()
            );
        }
    }

    public record UpdateResumeRequestDTO(
            List<String> techStack,  // 기술 스택
            String aboutMe,  // 자기소개
            List<String> tags,  // 태그 (회사명 등)
            List<WorkExperience> workExperiences,  // 경력
            List<Project> projects,  // 프로젝트
            List<Link> links,  // 개인 링크
            List<Education> educations,  // 학력
            List<Certificate> certificates  // 자격증
    ) {}

    public record CommentRequestDTO(
            @NotBlank(message = "내용은 비어있을 수 없습니다.") String content
    ) {}

    public record ResumeFilterDTO(
            PositionType position, // 직군별
            SchoolType schoolType, // 학력별
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

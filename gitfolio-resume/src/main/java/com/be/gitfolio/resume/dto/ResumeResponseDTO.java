package com.be.gitfolio.resume.dto;

import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.data.domain.Page;

import java.time.LocalDateTime;
import java.util.List;

public class ResumeResponseDTO {

    public record ResumeListDTO(
            String resumeId,
            Long memberId,
            String avatarUrl,
            PositionType position,
            String aboutMe,
            List<String> tags,
            int likeCount,
            int viewCount,
            @JsonProperty("isLiked") boolean liked
    ) {
        public ResumeListDTO(Resume resume, boolean liked) {
            this(
                    resume.getId(),
                    Long.parseLong(resume.getMemberId()),
                    resume.getAvatarUrl(),
                    resume.getPosition(),
                    resume.getAboutMe(),
                    resume.getTags(),
                    resume.getLikeCount(),
                    resume.getViewCount(),
                    liked
            );
        }
    }

    public record PaginationResponseDTO<T>(
            int currentPage,
            int totalPages,
            long totalElements,
            int size,
            List<T> content
    ) {
        public PaginationResponseDTO(Page<T> page) {
            this(
                    page.getNumber(),
                    page.getTotalPages(),
                    page.getTotalElements(),
                    page.getSize(),
                    page.getContent()
            );
        }
    }

    public record ResumeDetailDTO(
            String resumeId,
            Long memberId,
            String memberName,
            String avatarUrl,
            String email,
            PositionType position,
            List<String> techStack,
            String aboutMe,
            List<String> tags,
            List<Resume.WorkExperience> workExperiences,
            List<Resume.Project> projects,
            List<Resume.Link> links,
            List<Resume.Education> educations,
            List<Resume.Certificate> certificates,
            int likeCount,
            int viewCount,
            @JsonProperty("isLiked") boolean liked
    ) {
        public ResumeDetailDTO(Resume resume, boolean liked) {
            this(
                    resume.getId(),
                    Long.valueOf(resume.getMemberId()),
                    resume.getMemberName(),
                    resume.getAvatarUrl(),
                    resume.getEmail(),
                    resume.getPosition(),
                    resume.getTechStack(),
                    resume.getAboutMe(),
                    resume.getTags(),
                    resume.getWorkExperiences(),
                    resume.getProjects(),
                    resume.getLinks(),
                    resume.getEducations(),
                    resume.getCertificates(),
                    resume.getLikeCount(),
                    resume.getViewCount(),
                    liked
            );
        }
    }

    public record AIResponseDTO(
            List<Resume.Project> projects,
            List<String> techStack,
            String aboutMe
    ) {}

    public record CommentResponseDTO(
            Long id,
            String resumeId,
            Long memberId,
            String nickname,
            String avatarUrl,
            String content,
            LocalDateTime createdAt,
            LocalDateTime updatedAt
    ) {
        public CommentResponseDTO(Comment comment, String nickname, String avatarUrl) {
            this(
                    comment.getId(),
                    comment.getResumeId(),
                    comment.getMemberId(),
                    nickname,
                    avatarUrl,
                    comment.getContent(),
                    comment.getCreatedAt(),
                    comment.getUpdatedAt()
            );
        }
    }
}
package com.be.gitfolio.resume.dto;

import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.common.type.Visibility;
import com.be.gitfolio.common.utility.TimeUtils;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.type.Template;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;
import java.util.List;

import static com.be.gitfolio.resume.domain.Resume.*;

public class ResumeResponseDTO {

    public record UpdateVisibilityDTO(Visibility visibility) {}


    public record ResumeListDTO(
            String resumeId,
            Long memberId,
            String avatarUrl,
            PositionType position,
            String aboutMe,
            List<String> tags,
            int likeCount,
            int viewCount,
            LocalDateTime updatedAt,
            @JsonProperty("isLiked") boolean liked,
            Visibility visibility
    ) {
        public ResumeListDTO(Resume resume, boolean liked, String avatarFullUrl) {
            this(
                    resume.getId(),
                    Long.valueOf(resume.getMemberId()),
                    avatarFullUrl,
                    resume.getPosition(),
                    resume.getAboutMe(),
                    resume.getTags(),
                    resume.getLikeCount(),
                    resume.getViewCount(),
                    TimeUtils.convertUtcToSeoul(resume.getUpdatedAt()),
                    liked,
                    resume.getVisibility()
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
        public PaginationResponseDTO(List<T> content, long totalElements, Pageable pageable) {
            this(
                    pageable.getPageNumber(),
                    (int) Math.ceil((double) totalElements / pageable.getPageSize()),
                    totalElements,
                    pageable.getPageSize(),
                    content
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
            List<WorkExperience> workExperiences,
            List<Project> projects,
            List<Link> links,
            List<Education> educations,
            List<Certificate> certificates,
            int likeCount,
            int viewCount,
            @JsonProperty("isLiked") boolean liked,
            LocalDateTime updatedAt,
            Visibility visibility,
            Template template
    ) {
        public ResumeDetailDTO(Resume resume, boolean liked, String avatarFullUrl) {
            this(
                    resume.getId(),
                    Long.valueOf(resume.getMemberId()),
                    resume.getMemberName(),
                    avatarFullUrl,
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
                    liked,
                    TimeUtils.convertUtcToSeoul(resume.getUpdatedAt()),
                    resume.getVisibility(),
                    resume.getTemplate()
            );
        }
    }

    public record AIResponseDTO(
            List<Project> projects,
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
        public CommentResponseDTO(Comment comment, String nickname, String avatarFullUrl) {
            this(
                    comment.getId(),
                    comment.getResumeId(),
                    comment.getMemberId(),
                    nickname,
                    avatarFullUrl,
                    comment.getContent(),
                    TimeUtils.convertUtcToSeoul(comment.getCreatedAt()),
                    TimeUtils.convertUtcToSeoul(comment.getUpdatedAt())
            );
        }
    }
}
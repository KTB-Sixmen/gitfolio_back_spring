package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
public class Comment {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "comment_id")
    private Long id;

    private String resumeId;

    private Long memberId;

    private String content;

    private LocalDateTime createdAt;

    private LocalDateTime updatedAt;

    public static Comment of(String resumeId, Long senderId, CommentRequestDTO commentRequestDTO) {
        return Comment.builder()
                .resumeId(resumeId)
                .memberId(senderId)
                .content(commentRequestDTO.content())
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public void updateContent(String content) {
        this.content = content;
        this.updatedAt = LocalDateTime.now();
    }

    public void validateOwnership(Long memberId) {
        if (!this.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_COMMENT);
        }
    }
}

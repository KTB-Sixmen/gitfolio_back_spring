package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import jakarta.persistence.*;
import lombok.*;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
public class Comment extends BaseEntityMySQL {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "comment_id")
    private Long id;

    private String resumeId;

    private Long memberId;

    private String content;

    public void updateContent(String content) {
        this.content = content;
    }

    public void validateOwnership(Long memberId) {
        if (!this.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_COMMENT);
        }
    }
}

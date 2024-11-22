package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import jakarta.persistence.*;
import lombok.*;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
@Table(name = "resume_likes")
public class Like extends BaseEntityMySQL {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "like_id")
    private Long id;

    private Long memberId;

    private String resumeId;

    private Boolean status;

    public void updateStatus() {
        this.status = !this.status;
    }

    public static Like of(String resumeId, Long memberId) {
        return Like.builder()
                .resumeId(resumeId)
                .memberId(memberId)
                .status(Boolean.TRUE)
                .build();
    }
}

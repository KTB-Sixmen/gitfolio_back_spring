package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
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
    @Column(name = "like_id")
    private Long id;

    private String resumeId;

    private Long memberId;

    private String content;

    public void updateContent(String content) {
        this.content = content;
    }
}

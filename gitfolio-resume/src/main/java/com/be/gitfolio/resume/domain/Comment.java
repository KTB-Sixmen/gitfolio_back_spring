package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor
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

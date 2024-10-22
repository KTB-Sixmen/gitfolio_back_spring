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
}
package com.be.gitfolio.member.infrastructure.member;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.domain.Member;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
@Table(name = "member")
public class MemberEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "member_id")
    private Long id;

    private String username;

    private String nickname;

    private String name;

    private String githubName;

    private String role;

    private String avatarUrl;

    private String phoneNumber;

    private String email;

    @Enumerated(EnumType.STRING)
    private PositionType position;

    @Enumerated(EnumType.STRING)
    private PaidPlan paidPlan;

    private Integer remainingCount; // 잔여 사용 가능 횟수

    private LocalDateTime createdAt;

    private LocalDateTime updatedAt;

    public static MemberEntity fromModel(Member member) {
        return MemberEntity.builder()
                .id(member.getId())
                .username(member.getUsername())
                .nickname(member.getNickname())
                .name(member.getName())
                .githubName(member.getGithubName())
                .role(member.getRole())
                .avatarUrl(member.getAvatarUrl())
                .phoneNumber(member.getPhoneNumber())
                .email(member.getEmail())
                .position(member.getPosition())
                .paidPlan(member.getPaidPlan())
                .remainingCount(member.getRemainingCount())
                .createdAt(member.getCreatedAt())
                .updatedAt(member.getUpdatedAt())
                .build();
    }

    public Member toModel() {
        return Member.builder()
                .id(id)
                .username(username)
                .nickname(nickname)
                .name(name)
                .githubName(githubName)
                .role(role)
                .avatarUrl(avatarUrl)
                .phoneNumber(phoneNumber)
                .email(email)
                .position(position)
                .paidPlan(paidPlan)
                .remainingCount(remainingCount)
                .createdAt(createdAt)
                .updatedAt(updatedAt)
                .build();
    }

    public void resetCount() {
        this.remainingCount = 3;
    }
}

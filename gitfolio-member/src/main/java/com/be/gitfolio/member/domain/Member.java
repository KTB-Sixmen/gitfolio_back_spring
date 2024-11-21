package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import lombok.Builder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.util.Optional;

import static com.be.gitfolio.member.domain.MemberRequest.*;

@Builder
@Getter
@Slf4j
public class Member {

    private Long id;
    private String username;
    private String nickname;
    private String name;
    private String githubName;
    private String role;
    private String avatarUrl;
    private String phoneNumber;
    private String email;
    private PositionType position;
    private PaidPlan paidPlan;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static Member from(MemberCreate memberCreate) {
        log.info("회원 GithubName값 : {}", memberCreate.githubName());
        return Member.builder()
                .username(memberCreate.username())
                .nickname(memberCreate.nickname())
                .githubName(Optional.ofNullable(memberCreate.githubName()).orElse(""))
                .role(memberCreate.role())
                .avatarUrl(memberCreate.avatarUrl())
                .paidPlan(PaidPlan.FREE)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public Member updateMember(MemberUpdate memberUpdate, String avatarUrl) {
        return Member.builder()
                .id(id)
                .username(username)
                .nickname(nickname)
                .name(memberUpdate.name())
                .githubName(githubName)
                .role(role)
                .avatarUrl(avatarUrl)
                .phoneNumber(memberUpdate.phoneNumber())
                .email(memberUpdate.email())
                .position(memberUpdate.position())
                .paidPlan(paidPlan)
                .createdAt(createdAt)
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public Member updatePlan(PaidPlan paidPlan) {
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
                .createdAt(createdAt)
                .updatedAt(LocalDateTime.now())
                .build();
    }
}

package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import lombok.Builder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.util.Optional;

import static com.be.gitfolio.member.domain.request.MemberRequest.*;

@Builder(toBuilder = true)
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
    private Integer remainingCount;
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
                .remainingCount(3)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public Member updateMember(MemberUpdate memberUpdate, String avatarUrl) {
        return this.toBuilder()
                .name(memberUpdate.name())
                .avatarUrl(avatarUrl)
                .phoneNumber(memberUpdate.phoneNumber())
                .email(memberUpdate.email())
                .position(memberUpdate.position())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public Member updatePlan(PaidPlan paidPlan) {
        return this.toBuilder()
                .paidPlan(paidPlan)
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public Member decreaseRemainingCount() {
        if (this.paidPlan.equals(PaidPlan.FREE)) {
            return this.toBuilder()
                    .remainingCount(remainingCount - 1)
                    .updatedAt(LocalDateTime.now())
                    .build();
        } else {
            return this;
        }
    }
}

package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.dto.MemberRequestDTO;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;

@Getter
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Member extends BaseEntityMySQL {

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

    public void updatePlan(PaidPlan paidPlan) {
        this.paidPlan = paidPlan;
    }

    public static Member from(MemberCreateRequestDTO dto) {
        return Member.builder()
                .username(dto.username())
                .nickname(dto.nickname())
                .githubName(dto.githubName())
                .role(dto.role())
                .avatarUrl(dto.avatarUrl())
                .paidPlan(PaidPlan.FREE)
                .build();
    }


    /**
     * 회원 기본정보 수정
     */
    public void updateMember(MemberUpdateRequestDTO memberUpdateRequestDTO, String avatarUrl) {
        this.name = memberUpdateRequestDTO.name();
        this.avatarUrl = avatarUrl;
        this.phoneNumber = memberUpdateRequestDTO.phoneNumber();
        this.email = memberUpdateRequestDTO.email();
        this.position = memberUpdateRequestDTO.position();
    }

}

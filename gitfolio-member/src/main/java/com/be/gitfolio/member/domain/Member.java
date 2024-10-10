package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
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

    private String role;

    private String avatarUrl;

    private String phoneNumber;

    private String email;

    private String position;

    public static Member from(MemberCreateRequestDTO dto) {
        return Member.builder()
                .username(dto.getUsername())
                .nickname(dto.getNickname())
                .role(dto.getRole())
                .avatarUrl(dto.getAvatarUrl())
                .build();
    }


    /**
     * 회원 기본정보 수정
     */
    public void updateMember(MemberUpdateRequestDTO memberUpdateRequestDTO, String avatarUrl) {
        this.name = memberUpdateRequestDTO.getName();
        this.avatarUrl = avatarUrl;
        this.phoneNumber = memberUpdateRequestDTO.getPhoneNumber();
        this.email = memberUpdateRequestDTO.getEmail();
        this.position = memberUpdateRequestDTO.getPosition();
    }

}

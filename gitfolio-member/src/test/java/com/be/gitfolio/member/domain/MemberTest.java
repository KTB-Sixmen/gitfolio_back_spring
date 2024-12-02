package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import org.junit.jupiter.api.Test;

import static com.be.gitfolio.member.domain.request.MemberRequest.*;
import static org.assertj.core.api.Assertions.*;

public class MemberTest {

    @Test
    void Member는_MemberCreate로_생성할_수_있다() throws Exception {
        //given
        MemberCreate memberCreate = MemberCreate.builder()
                .avatarUrl("www.avatar.url.png")
                .githubName(null)
                .role("ROLE_USER")
                .nickname("namkikim0718")
                .username("123456789")
                .build();
        //when
        Member member = Member.from(memberCreate);
        //then
        assertThat(member.getAvatarUrl()).isEqualTo("www.avatar.url.png");
        assertThat(member.getGithubName()).isEmpty();
        assertThat(member.getRole()).isEqualTo("ROLE_USER");
        assertThat(member.getNickname()).isEqualTo("namkikim0718");
        assertThat(member.getUsername()).isEqualTo("123456789");
    }

    @Test
    void Member는_updateMember로_수정할_수_있다() throws Exception {
        //given
        Member member = Member.builder()
                .avatarUrl("www.avatar.url.png")
                .name("member")
                .position(PositionType.BACKEND)
                .name("memberName")
                .phoneNumber("01000000000")
                .email("member1234@gmail.com")
                .build();
        MemberUpdate updateMember = MemberUpdate.builder()
                .email("update1234@gmail.com")
                .phoneNumber("01011111111")
                .name("updateName")
                .position(PositionType.GAME)
                .build();
        //when
        member = member.updateMember(updateMember, "www.update.url.png");
        //then
        assertThat(member.getAvatarUrl()).isEqualTo("www.update.url.png");
        assertThat(member.getName()).isEqualTo(updateMember.name());
        assertThat(member.getPhoneNumber()).isEqualTo(updateMember.phoneNumber());
        assertThat(member.getPosition()).isEqualTo(updateMember.position());
        assertThat(member.getEmail()).isEqualTo(updateMember.email());
    }

    @Test
    void Member는_updatePlan으로_결제_플랜을_변경할_수_있다() throws Exception {
        //given
        Member member = Member.builder()
                .avatarUrl("www.avatar.url.png")
                .name("member")
                .position(PositionType.BACKEND)
                .name("memberName")
                .phoneNumber("01000000000")
                .email("member1234@gmail.com")
                .paidPlan(PaidPlan.FREE)
                .build();
        //when
        Member updatedMember = member.updatePlan(PaidPlan.PRO);
        //then
        assertThat(updatedMember.getPaidPlan()).isEqualTo(PaidPlan.PRO);
    }
}

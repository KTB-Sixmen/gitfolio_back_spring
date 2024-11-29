package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.mock.FakeMemberAdditionalRepository;
import com.be.gitfolio.member.mock.FakeMemberRepository;
import com.be.gitfolio.member.service.port.GithubApi;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.springframework.mock.web.MockMultipartFile;

import java.io.IOException;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

import static com.be.gitfolio.member.controller.response.MemberResponse.*;
import static com.be.gitfolio.member.domain.MemberAdditionalInfoRequest.*;
import static com.be.gitfolio.member.domain.MemberRequest.*;
import static org.assertj.core.api.Assertions.*;

class MemberServiceTest {

    private MemberServiceImpl memberService;
    private S3Service s3Service;
    private GithubApi githubApi;
    private FakeMemberRepository fakeMemberRepository;
    private FakeMemberAdditionalRepository fakeMemberAdditionalRepository;

    @BeforeEach
    void init() {
        fakeMemberRepository = new FakeMemberRepository();
        fakeMemberAdditionalRepository = new FakeMemberAdditionalRepository();
        this.s3Service = Mockito.mock(S3Service.class);
        this.githubApi = Mockito.mock(GithubApi.class);
        this.memberService = MemberServiceImpl.builder()
                .memberRepository(fakeMemberRepository)
                .memberAdditionalInfoRepository(fakeMemberAdditionalRepository)
                .s3Service(s3Service)
                .githubApi(githubApi)
                .build();
        fakeMemberRepository.save(Member.builder()
                .id(1L)
                .avatarUrl("avatarUrl.png")
                .username("memberUsername")
                .position(PositionType.BACKEND)
                .name("memberName")
                .phoneNumber("01000000000")
                .email("member1234@gmail.com")
                .paidPlan(PaidPlan.FREE)
                .build());
        fakeMemberRepository.save(Member.builder()
                .id(2L)
                .avatarUrl("avatars.githubusercontent.com")
                .username("memberUsername2")
                .position(PositionType.BACKEND)
                .name("memberName2")
                .phoneNumber("01000000001")
                .email("member2222@gmail.com")
                .paidPlan(PaidPlan.FREE)
                .build());
        fakeMemberAdditionalRepository.save(MemberAdditionalInfo.from(1L));
        fakeMemberAdditionalRepository.save(MemberAdditionalInfo.from(2L));
    }

    @Test
    void createMember는_Member와_MemberAdditionalInfo를_생성할_수_있다() throws Exception {
        //given
        MemberCreate memberCreate = MemberCreate.builder()
                .username("newUsername")
                .nickname("newNickName")
                .role("USER_ROLE")
                .githubName("newGithubName")
                .avatarUrl("www.new.png")
                .build();
        //when
        Member member = memberService.createMember(memberCreate);
        //then
        assertThat(member.getId()).isNotNull();
    }

    @Test
    void username으로_존재하는_회원의_기본정보를_가져올_수_있다() throws Exception {
        //given
        //when
        Optional<Member> member = memberService.findMemberIdByUsername("memberUsername");
        //then
        assertThat(member.get().getId()).isEqualTo(1L);
        assertThat(member.get().getName()).isEqualTo("memberName");
        assertThat(member.get().getPhoneNumber()).isEqualTo("01000000000");
    }

    @Test
    void 잘못된_username으로_기본정보를_조회하면_null을_반환한다() throws Exception {
        //given
        //when
        Optional<Member> member = memberService.findMemberIdByUsername("NotExistUsername");

        //then
        assertThat(member).isEmpty();
    }

    @Test
    void Resume모듈에는_프로필_이미지의_prefix_없는_값을_반환할_수_있다() throws Exception {
        //given
        //when
        MemberDetail memberDetail = memberService.sendMemberDetailToResume(1L);
        //then
        assertThat(memberDetail.avatarUrl()).isEqualTo("avatarUrl.png");
    }

    @Test
    void 잘못된_memberId로_회원정보를_resume_모듈에_보내려하면_에러를_던진다() throws Exception {
        //given
        //when
        //then
        assertThatThrownBy(() -> {
            memberService.sendMemberDetailToResume(77L);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void 상세_정보_조회시_s3이미지면_prefix를_붙인다() throws Exception {
        //given
        BDDMockito.given(s3Service.getFullFileUrl(ArgumentMatchers.anyString())).willReturn("https://gitfolio.s3.amazonaws.com/avatarUrl.png");
        //when
        MemberDetail memberDetail = memberService.getMemberDetails(1L);
        //then
        assertThat(memberDetail.avatarUrl()).isEqualTo("https://gitfolio.s3.amazonaws.com/avatarUrl.png");
    }

    @Test
    void 상세_정보_조회시_깃허브_이미지면_prefix를_적용하지_않는다() throws Exception {
        //given
        //when
        MemberDetail memberDetail = memberService.getMemberDetails(2L);
        //then
        assertThat(memberDetail.avatarUrl()).isEqualTo("avatars.githubusercontent.com");
    }

    @Test
    void 잘못된_memberId로_상세_정보_조회시_에러를_던진다() throws Exception {
        //given
        //when
        //then
        assertThatThrownBy(() -> {
            memberService.getMemberDetails(77L);
        }).isInstanceOf(BaseException.class);
    }

    @Test
    void 이미지_파일과_함께_회원의_기본_정보를_수정할_수_있다() throws Exception {
        //given
        Long memberId = 1L;
        MemberUpdate memberUpdate = MemberUpdate.builder()
                .name("newName")
                .email("newEmail")
                .position(PositionType.INFRA)
                .phoneNumber("01099999999")
                .build();
        MemberAdditionalInfoUpdate memberAdditionalInfoUpdate = MemberAdditionalInfoUpdate.builder()
                .workExperiences(Collections.emptyList())
                .educations(Collections.emptyList())
                .certificates(Collections.emptyList())
                .links(Collections.emptyList())
                .build();
        MockMultipartFile imageFile = new MockMultipartFile("image", "avatar.png", "image/png", "dummy data".getBytes());
        BDDMockito.given(s3Service.uploadFile(ArgumentMatchers.any(MockMultipartFile.class))).willReturn("new-avatar.png");
        //when
        memberService.updateMemberInfo(memberId, memberUpdate, memberAdditionalInfoUpdate, imageFile);
        //then
        Optional<Member> member = fakeMemberRepository.findById(1L);
        assertThat(member.get().getAvatarUrl()).isEqualTo("new-avatar.png");
        assertThat(member.get().getPosition()).isEqualTo(PositionType.INFRA);
    }

    @Test
    void 이미지_파일과_없이_회원의_기본_정보를_수정할_수_있다() throws Exception {
        //given
        Long memberId = 1L;
        MemberUpdate memberUpdate = MemberUpdate.builder()
                .name("newName")
                .email("newEmail")
                .position(PositionType.INFRA)
                .phoneNumber("01099999999")
                .build();
        MemberAdditionalInfoUpdate memberAdditionalInfoUpdate = MemberAdditionalInfoUpdate.builder()
                .workExperiences(Collections.emptyList())
                .educations(Collections.emptyList())
                .certificates(Collections.emptyList())
                .links(Collections.emptyList())
                .build();
        //when
        memberService.updateMemberInfo(memberId, memberUpdate, memberAdditionalInfoUpdate, null);
        //then
        Optional<Member> member = fakeMemberRepository.findById(1L);
        assertThat(member.get().getAvatarUrl()).isEqualTo("avatarUrl.png");
        assertThat(member.get().getPosition()).isEqualTo(PositionType.INFRA);
    }

    @Test
    void 회원의_결제_플랜을_변경할_수_있다() throws Exception {
        //given
        Long memberId = 1L;
        PaidPlan paidPlan = PaidPlan.STANDARD;
        //when
        memberService.updateMemeberPlan(memberId, paidPlan);
        //then
        Optional<Member> member = fakeMemberRepository.findById(1L);
        assertThat(member.get().getPaidPlan()).isEqualTo(PaidPlan.STANDARD);
    }

    @Test
    void 회원_정보를_삭제할_수_있다() throws Exception {
        //given
        Long memberId = 1L;
        //when
        memberService.deleteMember(memberId);
        //then
        Optional<Member> member = fakeMemberRepository.findById(memberId);
        assertThat(member.isEmpty()).isEqualTo(Boolean.TRUE);
    }
}
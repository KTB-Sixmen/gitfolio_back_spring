package com.be.gitfolio.member.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.member.controller.port.MemberService;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.request.MemberAdditionalInfoRequest.MemberAdditionalInfoUpdate;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static com.be.gitfolio.member.domain.request.MemberRequest.*;
import static com.be.gitfolio.member.controller.response.MemberResponse.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/members")
@Slf4j
public class MemberController {

    private final MemberService memberService;

    /**
     * 사용자 정보 조회(내부통신용)
     */
    @GetMapping("/{memberId}")
    public MemberDetail getMemberDetail(@PathVariable("memberId") Long memberId) {
        return memberService.sendMemberDetailToResume(memberId);
    }

    /**
     * 사용 플랜 변경(내부통신용)
     */
    @PatchMapping("/{memberId}/updatePlan")
    public Void updatePlan(@PathVariable("memberId") Long memberId, @RequestBody PaidPlan paidPlan) {
        return memberService.updateMemeberPlan(memberId, paidPlan);

    }

    /**
     * 회원 잔여 사용권 차감
     * @param memberId
     * @return
     */
    @PatchMapping("/{memberId}/remainingCount/decrease")
    public Void decreaseRemainingCount(@PathVariable("memberId") Long memberId) {
        return memberService.decreaseRemainingCount(memberId);
    }

    /**
     * 회원 생성
     */
    @PostMapping
    public ResponseEntity<BaseResponse<Long>> createMember(@RequestBody MemberCreate memberCreate) {
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "회원 가입에 성공했습니다.",
                memberService.createMember(memberCreate).getId()));
    }

    /**
     * 회원 ID(PK)를 username으로 조회
     */
    @GetMapping
    public ResponseEntity<BaseResponse<Long>> findMemberIdByUsername(@RequestParam("username") String username) {
        Optional<Member> member = memberService.findMemberIdByUsername(username);
        return member.map(value -> ResponseEntity.ok().body(new BaseResponse<>(value.getId())))
                .orElseGet(() -> ResponseEntity.ok().body(new BaseResponse<>(null)));
    }


    /**
     * 회원 정보 상세 조회
     */
    @AuthRequired
    @GetMapping("/me")
    public ResponseEntity<BaseResponse<MemberDetail>> getMemberDetails(HttpServletRequest request) {
        Long memberId = (Long) request.getAttribute("memberId");
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMemberDetails(memberId)));
    }

    /**
     * 회원 기본 정보 수정
     */
    @AuthRequired
    @PutMapping("/me")
    public ResponseEntity<BaseResponse<String>> updateMemberBasicInfo(
            HttpServletRequest request,
            @Valid @RequestPart("memberUpdateRequestDTO") MemberUpdate memberUpdate,
            @RequestPart("memberAdditionalRequestDTO") MemberAdditionalInfoUpdate memberAdditionalInfoUpdate,
            @RequestPart(value = "imageFile", required = false) MultipartFile imageFile) throws IOException {

        Long memberId = (Long) request.getAttribute("memberId");
        memberService.updateMemberInfo(memberId, memberUpdate, memberAdditionalInfoUpdate, imageFile);
        return ResponseEntity.ok().body(new BaseResponse<>("회원 기본 정보 수정이 완료되었습니다."));
    }

    /**
     * 회원 레포 조회
     */
    @AuthRequired
    @GetMapping("/myRepo")
    public ResponseEntity<BaseResponse<List<MemberGithubRepository>>> getUserRepositoriesWithLanguages(HttpServletRequest request) {
        String username = String.valueOf(request.getAttribute("nickname"));
        log.info("사용자 아이디 : {}", username);
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUserRepositoriesWithLanguages(username)));
    }

    /**
     * 회원 탈퇴
     */
    @AuthRequired
    @DeleteMapping("/me")
    public ResponseEntity<BaseResponse<String>> deleteMember(HttpServletRequest request) {
        Long memberId = (Long) request.getAttribute("memberId");
        memberService.deleteMember(memberId);
        return ResponseEntity.ok().body(new BaseResponse<>("회원 삭제가 완료되었습니다."));
    }
}

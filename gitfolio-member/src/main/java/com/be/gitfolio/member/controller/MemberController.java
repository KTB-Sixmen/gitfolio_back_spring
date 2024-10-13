package com.be.gitfolio.member.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.service.MemberService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;
import static com.be.gitfolio.member.dto.MemberResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/members")
public class MemberController {

    private final MemberService memberService;


    /**
     * 회원 생성
     */
    @PostMapping
    public ResponseEntity<BaseResponse<Long>> createMember(@RequestBody MemberCreateRequestDTO memberCreateRequestDTO) {
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "회원 가입에 성공했습니다.",
                memberService.createMember(memberCreateRequestDTO)));
    }

    /**
     * 회원 ID(PK)를 username으로 조회
     */
    @GetMapping
    public ResponseEntity<BaseResponse<Long>> findMemberIdByUsername(@RequestParam("username") String username) {
        Member member = memberService.findMemberIdByUsername(username);
        if (member != null) {
            return ResponseEntity.ok().body(new BaseResponse<>(member.getId()));
        } else {
            return ResponseEntity.ok().body(new BaseResponse<>(null));
        }
    }

    /**
     * 회원 추가 정보 생성
     */
    @AuthRequired
    @PostMapping("/additionalInfo")
    public ResponseEntity<BaseResponse<String>> createMemberAdditionalInfo(HttpServletRequest request,
                                                                           @RequestBody MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
        Long memberId = (Long) request.getAttribute("memberId");
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "회원 추가정보 생성에 성공했습니다.",
                memberService.createMemberAdditionalInfo(memberId, memberAdditionalRequestDTO)));
    }

    /**
     * 회원 추가 정보 수정
     */
    @AuthRequired
    @PutMapping("/additionalInfo")
    public ResponseEntity<BaseResponse<String>> updateMemberAdditionalInfo(HttpServletRequest request,
                                                                           @RequestBody MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
        Long memberId = (Long) request.getAttribute("memberId");
        memberService.updateMemberAdditionalInfo(memberId, memberAdditionalRequestDTO);
        return ResponseEntity.ok().body(new BaseResponse<>("회원 추가 정보 수정이 완료되었습니다."));
    }


    /**
     * 회원 정보 상세 조회
     */
    @AuthRequired
    @GetMapping("/me")
    public ResponseEntity<BaseResponse<MemberDetailDTO>> getMemberDetails(HttpServletRequest request) {
        Long memberId = (Long) request.getAttribute("memberId");
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMemberDetails(memberId)));
    }

    /**
     * 회원 기본 정보 수정
     */
    @AuthRequired
    @PutMapping("/me")
    public ResponseEntity<BaseResponse<String>> updateMemberBasicInfo(HttpServletRequest request,
                                                                      @RequestPart("memberUpdateRequestDTO") MemberUpdateRequestDTO memberUpdateRequestDTO,
                                                                      @RequestPart("imageFile") MultipartFile imageFile) throws IOException {

        Long memberId = (Long) request.getAttribute("memberId");
        memberService.updateMemberBasicInfo(memberId, memberUpdateRequestDTO, imageFile);
        return ResponseEntity.ok().body(new BaseResponse<>("회원 기본 정보 수정이 완료되었습니다."));
    }
}

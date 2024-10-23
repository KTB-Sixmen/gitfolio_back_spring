package com.be.gitfolio.member.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.service.MemberService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.A;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;
import static com.be.gitfolio.member.dto.MemberResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/members")
@Slf4j
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
    public ResponseEntity<BaseResponse<String>> updateMemberBasicInfo(
            HttpServletRequest request,
            @RequestPart("memberUpdateRequestDTO") MemberUpdateRequestDTO memberUpdateRequestDTO,
            @RequestPart("memberAdditionalRequestDTO") MemberAdditionalRequestDTO memberAdditionalRequestDTO,
            @RequestPart(value = "imageFile", required = false) MultipartFile imageFile) throws IOException {

        Long memberId = (Long) request.getAttribute("memberId");
        memberService.updateMemberInfo(memberId, memberUpdateRequestDTO, memberAdditionalRequestDTO, imageFile);
        return ResponseEntity.ok().body(new BaseResponse<>("회원 기본 정보 수정이 완료되었습니다."));
    }

    /**
     * 회원 레포 조회
     */
    @AuthRequired
    @GetMapping("/myRepo")
    public ResponseEntity<BaseResponse<List<MemberGithubRepositoryDTO>>> getUserRepositoriesWithLanguages(HttpServletRequest request) {
        String username = String.valueOf(request.getAttribute("nickname"));
        log.info("사용자 아이디 : {}", username);
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUserRepositoriesWithLanguages(username)));
    }
}

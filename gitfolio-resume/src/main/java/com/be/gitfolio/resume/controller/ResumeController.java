package com.be.gitfolio.resume.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.service.ResumeService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/resumes")
public class ResumeController {

    private final ResumeService resumeService;

    /**
     * 이력서 생성 요청
     */
    @AuthRequired
    @PostMapping
    public ResponseEntity<BaseResponse<String>> createResume(HttpServletRequest request,
                                                             @RequestBody CreateResumeRequestDTO createResumeRequestDTO) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "이력서 생성에 성공했습니다.",
                resumeService.createResume(memberId, createResumeRequestDTO)
        ));
    }

    /**
     * 이력서 목록 조회
     */
    @GetMapping
    public ResponseEntity<BaseResponse<List<ResumeListDTO>>> getResumeList() {
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getResumeList()));
    }

    /**
     * 이력서 상세 조회
     */
    @GetMapping("/{resumeId}")
    public ResponseEntity<BaseResponse<ResumeDetailDTO>> getResumeDetail(@PathVariable("resumeId") String resumeId,
                                                                         HttpServletRequest request) {
        String clientIp = getClientIp(request);
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getResumeDetail(resumeId, clientIp)));
    }

    // 클라이언트 IP 가져오는 유틸리티 메서드
    private String getClientIp(HttpServletRequest request) {
        String ip = request.getHeader(("X-Forwarded-For"));
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }

    /**
     * 내 이력서 목록 조회
     */
    @AuthRequired
    @GetMapping("/me")
    public ResponseEntity<BaseResponse<List<ResumeListDTO>>> getMyResumeList(HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getMyResumeList(memberId)));

    }

    /**
     * 이력서 단건 삭제
     */
    @AuthRequired
    @DeleteMapping("/{resumeId}")
    public ResponseEntity<BaseResponse<String>> deleteResume(@PathVariable("resumeId") String resumeId) {
        resumeService.deleteResume(resumeId);
        return ResponseEntity.ok().body(new BaseResponse<>("이력서 삭제가 완료되었습니다."));
    }

    /**
     * 이력서 직접 수정
     */
    @AuthRequired
    @PutMapping("/{resumeId}")
    public ResponseEntity<BaseResponse<String>> updateResume(@PathVariable("resumeId") String resumeId, @RequestBody UpdateResumeRequestDTO updateResumeRequestDTO) {
        resumeService.updateResume(resumeId, updateResumeRequestDTO);
        return ResponseEntity.ok().body(new BaseResponse<>("이력서 수정이 완료되었습니다."));
    }

}

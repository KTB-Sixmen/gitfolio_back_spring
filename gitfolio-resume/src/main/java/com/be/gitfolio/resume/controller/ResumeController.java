package com.be.gitfolio.resume.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.service.CommentService;
import com.be.gitfolio.resume.service.ResumeService;
import jakarta.persistence.OptimisticLockException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/resumes")
public class ResumeController {

    private final ResumeService resumeService;
    private final CommentService commentService;

    /**
     * 이력서 생성 요청
     */
    @AuthRequired
    @PostMapping()
    public ResponseEntity<BaseResponse<String>> createResume(HttpServletRequest request,
                                                             @Valid @RequestBody CreateResumeRequestDTO createResumeRequestDTO
    ) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "이력서 생성에 성공했습니다.",
                resumeService.createResume(memberId, createResumeRequestDTO, createResumeRequestDTO.visibility())
        ));
    }

    /**
     * 이력서 수정
     */
    @AuthRequired
    @PostMapping("/{resumeId}")
    public ResponseEntity<BaseResponse<String>> update(HttpServletRequest request,
                                                       @PathVariable("resumeId") String resumeId,
                                                       @Valid @RequestBody UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.updateResumeWithAI(memberId, resumeId, updateResumeWithAIRequestDTO)));
    }

    /**
     * 이력서 공개 여부 변경
     */
    @AuthRequired
    @PatchMapping("/{resumeId}/visibility")
    public ResponseEntity<BaseResponse<String>> updateVisibility(HttpServletRequest request,
                                                                 @PathVariable("resumeId") String resumeId,
                                                                 @RequestBody UpdateVisibilityDTO updateVisibilityDTO) {
        String memberId = request.getAttribute("memberId").toString();
        resumeService.updateVisibility(Long.valueOf(memberId), resumeId, updateVisibilityDTO);
        return ResponseEntity.ok().body(new BaseResponse<>("이력서 공개 여부가 변경되었습니다."));
    }


    /**
     * 이력서 목록 조회
     */
    @GetMapping()
    public ResponseEntity<BaseResponse<PaginationResponseDTO<ResumeListDTO>>> getResumeList(
            @RequestParam(required = false) String tag,
            @RequestParam(required = false) String position,
            @RequestParam(required = false) String techStack,
            @RequestParam(required = false) String schoolType,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "recent") String sortOrder,
            @RequestParam(required = false) Boolean liked,
            @RequestHeader(value = "Authorization", required = false) String token
    ) {
        ResumeFilterDTO resumeFilterDTO = new ResumeFilterDTO(
                tag,
                position,
                techStack,
                schoolType,
                sortOrder,
                liked,
                page,
                size
        );

        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getResumeList(token, resumeFilterDTO)));
    }

    /**
     * 이력서 상세 조회
     */
    @GetMapping("/{resumeId}")
    public ResponseEntity<BaseResponse<ResumeDetailDTO>> getResumeDetail(@PathVariable("resumeId") String resumeId,
                                                                         @RequestHeader(value = "Authorization", required = false) String token,
                                                                         HttpServletRequest request) {
        String clientIp = getClientIp(request);
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getResumeDetail(token, resumeId, clientIp)));
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
    public ResponseEntity<BaseResponse<PaginationResponseDTO<ResumeListDTO>>> getMyResumeList(HttpServletRequest request,
                                                                             @RequestParam(defaultValue = "0") int page,
                                                                             @RequestParam(defaultValue = "10") int size) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.ok().body(new BaseResponse<>(resumeService.getMyResumeList(Long.valueOf(memberId), page, size)));

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
    public ResponseEntity<BaseResponse<String>> updateResume(HttpServletRequest request,
                                                             @PathVariable("resumeId") String resumeId,
                                                             @RequestPart("updateResumeRequestDTO") UpdateResumeRequestDTO updateResumeRequestDTO,
                                                             @RequestPart(value = "imageFile", required = false) MultipartFile imageFile) throws IOException {

        String memberId = request.getAttribute("memberId").toString();
        resumeService.updateResume(memberId,resumeId, updateResumeRequestDTO, imageFile);
        return ResponseEntity.ok().body(new BaseResponse<>("이력서 수정이 완료되었습니다."));
    }

    /**
     * 좋아요 기능
     */
    @AuthRequired
    @PostMapping("/{resumeId}/likes")
    public ResponseEntity<BaseResponse<String>> toggleLike(@PathVariable("resumeId") String resumeId,
                                                           HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        boolean liked = resumeService.toggleLike(resumeId, Long.valueOf(memberId));
        if (liked) {
            return ResponseEntity.ok().body(new BaseResponse<>("좋아요가 추가되었습니다."));
        } else {
            return ResponseEntity.ok().body(new BaseResponse<>("좋아요 상태가 변경되었습니다."));
        }
    }

//    // 테스트용(kafka)
//    @PostMapping("/{resumeId}/commentsWithKafka")
//    public ResponseEntity<BaseResponse<Long>> createCommentTestKafka(@PathVariable("resumeId") String resumeId,
//                                                            HttpServletRequest request,
//                                                            @Valid @RequestBody CommentRequestDTO commentRequestDTO) {
//        Long memberId = 2L;
//        String nickname = "namkikim0718";
//        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
//                HttpStatus.CREATED,
//                "201 CREATED",
//                "댓글 생성에 성공했습니다.",
//                commentService.createComment(resumeId, memberId, nickname, commentRequestDTO)
//        ));
//    }
//
//    // 테스트용(webClient)
//    @PostMapping("/{resumeId}/commentsWithHttp")
//    public ResponseEntity<BaseResponse<Long>> createCommentTestWebClient(@PathVariable("resumeId") String resumeId,
//                                                                HttpServletRequest request,
//                                                                @Valid @RequestBody CommentRequestDTO commentRequestDTO) {
//        Long memberId = 2L;
//        String nickname = "namkikim0718";
//        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
//                HttpStatus.CREATED,
//                "201 CREATED",
//                "댓글 생성에 성공했습니다.",
//                commentService.createCommentWithHttp(resumeId, memberId, nickname, commentRequestDTO)
//        ));
//    }

    /**
     * 댓글 작성
     */
    @AuthRequired
    @PostMapping("/{resumeId}/comments")
    public ResponseEntity<BaseResponse<Long>> createComment(@PathVariable("resumeId") String resumeId,
                                                            HttpServletRequest request,
                                                            @Valid @RequestBody CommentRequestDTO commentRequestDTO) {
        String senderId = request.getAttribute("memberId").toString();
        String senderNickname = request.getAttribute("nickname").toString();
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "댓글 생성에 성공했습니다.",
                commentService.createComment(resumeId, Long.valueOf(senderId), senderNickname, commentRequestDTO)
        ));
    }

    /**
     * 댓글 수정
     */
    @AuthRequired
    @PatchMapping("/comments/{commentId}")
    public ResponseEntity<BaseResponse<String>> updateComment(@PathVariable("commentId") Long commentId,
                                                              HttpServletRequest request,
                                                              @Valid @RequestBody CommentRequestDTO commentRequestDTO) {
        String memberId = request.getAttribute("memberId").toString();
        commentService.updateComment(commentId, Long.valueOf(memberId), commentRequestDTO);
        return ResponseEntity.ok().body(new BaseResponse<>("댓글이 수정되었습니다."));
    }

    /**
     * 댓글 삭제
     */
    @AuthRequired
    @DeleteMapping("/comments/{commentId}")
    public ResponseEntity<BaseResponse<String>> deleteComment(@PathVariable("commentId") Long commentId,
                                                              HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        commentService.deleteComment(commentId, Long.valueOf(memberId));
        return ResponseEntity.ok().body(new BaseResponse<>("댓글 삭제가 완료되었습니다."));
    }

    /**
     * 이력서별 댓글 조회
     */
    @GetMapping("/{resumeId}/comments")
    public ResponseEntity<BaseResponse<List<CommentResponseDTO>>> getCommentList(@PathVariable("resumeId") String resumeId) {
        return ResponseEntity.ok().body(new BaseResponse<>(commentService.getCommentList(resumeId)));
    }
}

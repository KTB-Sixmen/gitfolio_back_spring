package com.be.gitfolio.chat.controller;

import com.be.gitfolio.chat.domain.ChatMessage;
import com.be.gitfolio.chat.dto.MessageRequestDTO;
import com.be.gitfolio.chat.dto.MessageResponseDTO;
import com.be.gitfolio.chat.service.ChatService;
import com.be.gitfolio.chat.service.ChatStreamService;
import com.be.gitfolio.common.aop.AuthRequired;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static com.be.gitfolio.chat.dto.MessageRequestDTO.*;
import static com.be.gitfolio.chat.dto.MessageResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/chat")
@Slf4j
public class ChatController {

    private final ChatService chatService;
    private final ChatStreamService chatStreamService;

    /**
     * 메시지 전송
     */
    @AuthRequired
    @PostMapping("/send")
    public Mono<Void> sendMessage(HttpServletRequest request,
                            @RequestBody MessageContentDTO messageContentDTO) {
        log.info("/send 컨트롤러 진입");
        String memberId = request.getAttribute("memberId").toString();
        String nickname = request.getAttribute("nickname").toString();

        // 메시지를 먼저 저장하고 성공하면 스트림에 발행
        return chatService.sendMessage(Long.valueOf(memberId), nickname, messageContentDTO.content())
                .doOnSuccess(chatMessage -> {
                    log.info("Message saved successfully, now publishing to stream");
                    MessageDetailDTO messageDetailDTO = MessageDetailDTO.from(chatMessage);
                    chatStreamService.publish(messageDetailDTO);
                    log.info("publish 완료");
                })
                .doOnError(error -> log.error("Failed to send message: ", error))
                .then();
    }

    /**
     * 실시간 메시지 스트림
     */
    @GetMapping(value = "/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<MessageDetailDTO> streamMessages() {
        return chatStreamService.getMessageStream();
    }

    /**
     * 채팅방 입장
     */
    @AuthRequired
    @PostMapping("/enter")
    @ResponseStatus(HttpStatus.OK)
    public Mono<Void> memberJoin(HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        return chatService.memberJoin(Long.valueOf(memberId));
    }

    /**
     * 채팅방 퇴장
     */
    @AuthRequired
    @PostMapping("/exit")
    @ResponseStatus(HttpStatus.OK)
    public Mono<Void> memberLeave(HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        return chatService.memberLeave(Long.valueOf(memberId));
    }

    /**
     * 채팅 히스토리 조회
     */
    @GetMapping("/history")
    public Flux<ChatMessage> getChatHistory() {
        return chatService.getChathistory();
    }

    /**
     * 채팅 참여자 수 조회
     */
    @GetMapping("/participants/count")
    public Mono<Integer> getParticipantCount() {
        return chatService.getParticipantCount();
    }
}

package com.be.gitfolio.chat.service;

import com.be.gitfolio.chat.domain.ChatMessage;
import com.be.gitfolio.chat.domain.ChatRoom;
import com.be.gitfolio.chat.domain.MemberSession;
import com.be.gitfolio.chat.repository.ChatMessageRepository;
import com.be.gitfolio.chat.repository.ChatRoomRepository;
import com.be.gitfolio.chat.repository.MemberSessionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatService {

    private final ChatMessageRepository chatMessageRepository;
    private final ChatRoomRepository chatRoomRepository;
    private final MemberSessionRepository memberSessionRepository;

    /**
     * 채팅방 입장
     */
    public Mono<Void> memberJoin(Long memberId) {
        return memberSessionRepository.findByMemberId(memberId)
                .switchIfEmpty(Mono.defer(() -> {
                    // 새로운 세션 생성
                    MemberSession newSession = MemberSession.from(memberId);
                    return memberSessionRepository.save(newSession);
                }))
                .flatMap(existingSession -> {
                    if (!existingSession.isActive()) {
                        existingSession.updateJoinedAt();
                        log.info("입장 시간 변경! 퇴장시간 초기화!");
                        return memberSessionRepository.save(existingSession);
                    }
                    return Mono.just(existingSession);  // 이미 활성 상태면 갱신하지 않음
                })
                .then(chatRoomRepository.findById("globalChatRoom")
                        .defaultIfEmpty(new ChatRoom("globalChatRoom"))
                        .flatMap(chatRoom -> {
                            chatRoom.addParticipant(memberId);
                            return chatRoomRepository.save(chatRoom);
                        }))
                .then();
    }

    /**
     * 채팅방 퇴장
     */
    public Mono<Void> memberLeave(Long memberId) {
        return memberSessionRepository.findByMemberId(memberId)
                .flatMap(existingSession -> {
                    existingSession.markAsLeft();
                    return memberSessionRepository.save(existingSession);
                })
                .then(chatRoomRepository.findById("globalChatRoom")
                        .flatMap(chatRoom -> {
                            chatRoom.removeParticipant(memberId);
                            return chatRoomRepository.save(chatRoom);
                        }))
                .then();
    }

    /**
     * 메시지 보내기
     */
    public Mono<ChatMessage> sendMessage(Long memberId, String nickname, String content) {
        ChatMessage chatMessage = ChatMessage.builder()
                .senderId(memberId)
                .senderNickname(nickname)
                .content(content)
                .sentAt(LocalDateTime.now())
                .build();
        return chatMessageRepository.save(chatMessage);
    }

    /**
     * 채팅 히스토리 조회
     */
    public Flux<ChatMessage> getChathistory() {
        return chatMessageRepository.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
    }

    /**
     * 채팅 참여자 수 조회
     */
    public Mono<Integer> getParticipantCount() {
        return chatRoomRepository.findById("globalChatRoom")
                .map(ChatRoom::getParticipantCount)
                .defaultIfEmpty(0);
    }
}

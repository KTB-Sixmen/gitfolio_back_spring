package com.be.gitfolio.chat.service;

import com.be.gitfolio.chat.dto.MessageResponseDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import static com.be.gitfolio.chat.dto.MessageResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatStreamService {

    private final KafkaTemplate<String, MessageDetailDTO> kafkaTemplate;
    private final Sinks.Many<MessageDetailDTO> sink = Sinks.many().multicast().onBackpressureBuffer();

    /**
     * Kafka 메시지 발행
     */
    public void publish(MessageDetailDTO message) {
        kafkaTemplate.send("chatTopic", message);
    }

    /**
     * Kafka 메시지 수신
     */
    @KafkaListener(topics = "chatTopic", groupId = "chat-group")
    public void consume(MessageDetailDTO message) {
        sink.tryEmitNext(message);
    }

    // WebFlux를 통해 클라이언트로 스트림 전송
    public Flux<MessageDetailDTO> getMessageStream() {
        return sink.asFlux();
    }
}

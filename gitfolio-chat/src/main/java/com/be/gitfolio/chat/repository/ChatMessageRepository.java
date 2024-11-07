package com.be.gitfolio.chat.repository;

import com.be.gitfolio.chat.domain.ChatMessage;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ChatMessageRepository extends ReactiveMongoRepository<ChatMessage, String> {}

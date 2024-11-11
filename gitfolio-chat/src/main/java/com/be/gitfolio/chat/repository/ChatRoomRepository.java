package com.be.gitfolio.chat.repository;

import com.be.gitfolio.chat.domain.ChatRoom;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ChatRoomRepository extends ReactiveMongoRepository<ChatRoom, String> {
}

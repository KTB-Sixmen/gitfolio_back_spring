package com.be.gitfolio.auth.service.port;

import reactor.core.publisher.Mono;

import static com.be.gitfolio.auth.dto.MemberDTO.*;

public interface MemberClient {

    Mono<Long> findMemberIdByUsername(String username);

    Mono<Long> createMemberInMemberModule(MemberSaveRequestDTO memberSaveRequestDTO);

}

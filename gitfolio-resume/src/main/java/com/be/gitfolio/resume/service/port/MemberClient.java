package com.be.gitfolio.resume.service.port;

import reactor.core.Disposable;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public interface MemberClient {

    MemberInfoDTO getMemberInfo(Long memberId);

    Disposable decreaseRemainingCount(Long memberId);

}

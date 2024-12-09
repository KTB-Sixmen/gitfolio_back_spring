package com.be.gitfolio.resume.mock;

import com.be.gitfolio.resume.service.port.MemberClient;
import reactor.core.Disposable;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public class FakeMemberClient implements MemberClient {

    private MemberInfoDTO memberInfoDTO;

    public void setMemberInfoDTO(MemberInfoDTO memberInfoDTO) {
        this.memberInfoDTO = memberInfoDTO;
    }

    @Override
    public MemberInfoDTO getMemberInfo(Long memberId) {
        return memberInfoDTO;
    }

    @Override
    public Disposable decreaseRemainingCount(Long memberId) {
        return new Disposable() {
            @Override
            public void dispose() {
                // 가짜 dispose 동작
            }

            @Override
            public boolean isDisposed() {
                return false; // 항상 false로 반환 (테스트용)
            }
        };
    }
}

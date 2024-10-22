package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.grpc.MemberServiceGrpc;
import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.repository.MemberRepository;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import org.lognet.springboot.grpc.GRpcService;

import java.util.Optional;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

@GRpcService
@RequiredArgsConstructor
public class MemberServiceGrpcImpl extends MemberServiceGrpc.MemberServiceImplBase {

    private final MemberRepository memberRepository;

    @Override
    public void getMemberById(MemberRequestById request, StreamObserver<MemberResponseById> responseObserver) {
        String memberId = request.getMemberId();

        Optional<Member> memberOpt = memberRepository.findById(Long.valueOf(memberId));

        if (memberOpt.isPresent()) {
            Member member = memberOpt.get();

            MemberResponseById response = MemberResponseById.newBuilder()
                    .setMemberId(String.valueOf(member.getId()))
                    .setNickname(member.getNickname())
                    .build();

            responseObserver.onNext(response);
            responseObserver.onCompleted();
        } else {
            StatusRuntimeException statusException = Status.NOT_FOUND
                    .withDescription("Member Not Found with ID:" + memberId)
                    .asRuntimeException();
            responseObserver.onError(statusException);
        }


    }
}

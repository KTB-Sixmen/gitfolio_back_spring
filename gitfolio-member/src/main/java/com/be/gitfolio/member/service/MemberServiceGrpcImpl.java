package com.be.gitfolio.member.service;

import com.be.gitfolio.common.grpc.MemberServiceGrpc;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.mapper.*;
import com.be.gitfolio.member.repository.MemberAdditionalInfoRepository;
import com.be.gitfolio.member.repository.MemberRepository;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import org.lognet.springboot.grpc.GRpcService;

import java.util.Optional;
import java.util.stream.Collectors;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

@GRpcService
@RequiredArgsConstructor
public class MemberServiceGrpcImpl extends MemberServiceGrpc.MemberServiceImplBase {

    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;

    @Override
    public void getMemberById(MemberRequestById request, StreamObserver<MemberResponseById> responseObserver) {
        String memberId = request.getMemberId();

        Optional<Member> memberOpt = memberRepository.findById(Long.valueOf(memberId));
        Optional<MemberAdditionalInfo> memberAdditionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(memberId);

        if (memberOpt.isPresent() && memberAdditionalInfoOpt.isPresent()) {
            Member member = memberOpt.get();
            MemberAdditionalInfo memberAdditionalInfo = memberAdditionalInfoOpt.get();

            MemberResponseById response = MemberResponseById.newBuilder()
                    .setMemberId(String.valueOf(member.getId()))
                    .setNickname(member.getNickname())
                    .setMemberName(member.getName())
                    .setAvatarUrl(member.getAvatarUrl())
                    .setPhoneNumber(member.getPhoneNumber())
                    .setEmail(member.getEmail())
                    .setPosition(member.getPosition())
                    .addAllWorkExperiences(
                            memberAdditionalInfo.getWorkExperiences().stream()
                            .map(ProtoMapper::toProto)
                            .collect(Collectors.toList())
                    )
                    .addAllEducations(
                            memberAdditionalInfo.getEducations().stream()
                            .map(ProtoMapper::toProto)
                            .collect(Collectors.toList())
                    )
                    .addAllCertificates(
                            memberAdditionalInfo.getCertificates().stream()
                                    .map(ProtoMapper::toProto)
                                    .collect(Collectors.toList())
                    )
                    .addAllActivities(
                            memberAdditionalInfo.getActivities().stream()
                                    .map(ProtoMapper::toProto)
                                    .collect(Collectors.toList())
                    )
                    .addAllLinks(
                            memberAdditionalInfo.getLinks().stream()
                            .map(ProtoMapper::toProto)
                            .collect(Collectors.toList())
                    )
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

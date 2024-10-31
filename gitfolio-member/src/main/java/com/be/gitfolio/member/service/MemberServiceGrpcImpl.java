package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.grpc.MemberServiceGrpc;
import com.be.gitfolio.common.type.PaidPlan;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.stream.Collectors;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

@GRpcService
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class MemberServiceGrpcImpl extends MemberServiceGrpc.MemberServiceImplBase {

    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;

    @Override
    @Transactional
    public void updateMemberPlan(UpdatePlanRequest request, StreamObserver<UpdatePlanResponse> responseObserver) {
        try {
            String memberId = request.getMemberId();
            Member member = memberRepository.findById(Long.valueOf(memberId))
                    .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

            // enum 타입으로 변경해서 저장
            member.updatePlan(PaidPlan.fromPlanName(request.getPaidPlan()));

            // 성공 응답 전송
            UpdatePlanResponse response = UpdatePlanResponse.newBuilder()
                    .setSuccess(true)
                    .setMessage("Plan updated successfully")
                    .build();
            responseObserver.onNext(response);
            responseObserver.onCompleted();

        } catch (BaseException e) {
            // 커스텀 예외의 상세 정보를 포함하여 gRPC 상태와 함께 전송
            StatusRuntimeException grpcException = Status.INVALID_ARGUMENT
                    .withDescription("Error updating plan: " + e.getMessage())
                    .augmentDescription("Error Code: " + e.getErrorCode()) // 필요시 코드 추가
                    .asRuntimeException();

            responseObserver.onError(grpcException);
        } catch (Exception e) {
            // 일반적인 서버 오류 처리
            StatusRuntimeException grpcException = Status.INTERNAL
                    .withDescription("Unexpected error occurred: " + e.getMessage())
                    .asRuntimeException();

            responseObserver.onError(grpcException);
        }
    }

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
                    .setGithubName(member.getGithubName())
                    .setAvatarUrl(member.getAvatarUrl())
                    .setPhoneNumber(member.getPhoneNumber())
                    .setEmail(member.getEmail())
                    .setPosition(member.getPosition())
                    .addAllWorkExperiences(
                            memberAdditionalInfo.getWorkExperiences().stream()
                                    .map(ProtoMapper::toProto)
                                    .toList()
                    )
                    .addAllEducations(
                            memberAdditionalInfo.getEducations().stream()
                                    .map(ProtoMapper::toProto)
                                    .toList()
                    )
                    .addAllCertificates(
                            memberAdditionalInfo.getCertificates().stream()
                                    .map(ProtoMapper::toProto)
                                    .toList()
                    )
                    .addAllLinks(
                            memberAdditionalInfo.getLinks().stream()
                                    .map(ProtoMapper::toProto)
                                    .toList()
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

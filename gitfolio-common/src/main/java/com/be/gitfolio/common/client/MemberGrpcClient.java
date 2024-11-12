package com.be.gitfolio.common.client;

import com.be.gitfolio.common.grpc.MemberServiceGrpc;
import com.be.gitfolio.common.grpc.MemberServiceProto;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder;
import org.springframework.stereotype.Service;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;

@Service
public class MemberGrpcClient {

    private final MemberServiceGrpc.MemberServiceBlockingStub memberStub;

    public MemberGrpcClient() {
        // gRPC 채널 설정 (포트 9090) - NettyChannelBuilder 사용
        ManagedChannel channel = ManagedChannelBuilder.forAddress("localhost", 9090)
                .usePlaintext()
                .build();
        memberStub = MemberServiceGrpc.newBlockingStub(channel);
    }

    public MemberResponseById getMember(String memberId) {
        MemberRequestById request = MemberRequestById.newBuilder()
                .setMemberId(memberId)
                .build();
        return memberStub.getMemberById(request);
    }

    public UpdatePlanResponse updateMemberPlan(String memberId, String paidPlan) {
        UpdatePlanRequest request = UpdatePlanRequest.newBuilder()
                .setMemberId(memberId)
                .setPaidPlan(paidPlan)
                .build();
        return memberStub.updateMemberPlan(request);
    }
}
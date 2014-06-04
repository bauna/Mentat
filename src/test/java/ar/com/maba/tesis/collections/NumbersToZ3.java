package ar.com.maba.tesis.collections;

import static java.util.Arrays.asList;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

@SuppressWarnings("unused")
public class NumbersToZ3 {

	private Boolean bbool = true;
	private Byte bbyte = 2;
	private Short bshort = 2;
	private Integer bint = 1;
	private Long blong = 2L;
	private BigInteger bigInteger = BigInteger.TEN;
	private Float bfloat = 3.2F;
	private Double bdouble = 3.1;
	private BigDecimal bigDecimal = BigDecimal.TEN;

	private boolean sbool = true;
	private byte sbyte = 10;
	private short sshort = 10;
	private int sint = 10;
	private long slong = 10;
	private float sfloat = 4.1F;
	private double sdouble = 4.2;

	private List<Boolean> listBool = new ArrayList<>(asList(new Boolean[] { true, true, false, true }));
	private List<Byte> listByte = new ArrayList<>(asList(new Byte[] { 2, 1, 2}));
	private List<Short> listShort = new ArrayList<>(asList(new Short[] { 1, 2, 3, 4 }));
	private List<Integer> listInt = new ArrayList<>(asList(new Integer[] { 1,	2, 3, 4 }));
	private List<Long> listLong = new ArrayList<>(asList(new Long[] { 1L, 2L, 3L, 4L }));
	private List<BigInteger> listBigInteger = new ArrayList<>(asList(new BigInteger[] { BigInteger.TEN, BigInteger.ONE }));
	private List<Float> listFloat = new ArrayList<>(asList(new Float[] { 3.2F, 2.5F, 18.653F }));
	private List<Double> listDouble = new ArrayList<>(asList(new Double[] { new Double(3.3), new Double(5.32), new Double(23.12) }));
	private List<BigDecimal> listBigDecimal = new ArrayList<>(asList(new BigDecimal[] { BigDecimal.TEN, BigDecimal.ZERO }));
	private ar.com.maba.tesis.arrayList.ArrayList<Boolean> listBool2 = new ar.com.maba.tesis.arrayList.ArrayList<>(asList(new Boolean[] { true, true, false, true }));
	private Integer[] arrayInt = { 1, 2, 3, 4 };

	private boolean[] listBoll = { true , false };
	private int[] listInt1 = { 1, 2, 3, 4 };
	private byte[] listByte1 = { 1, 2, 3, 4 };
	private long[] listLong1 = { 1, 2, 3, 4 };
	private float[] listFloat1 = { 1, 2, 3, 4 };
	private double[] listDouble1 = { 1, 2, 3, 4 };

}

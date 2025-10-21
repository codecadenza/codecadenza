/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.model.testing.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Integration Method Test Invocation</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#isExpectToFail <em>Expect To
 * Fail</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getPostProcessingStatement <em>Post
 * Processing Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getTimeout <em>Timeout</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getIntegrationMethod <em>Integration
 * Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getParameters
 * <em>Parameters</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getReturnValues <em>Return
 * Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getPostProcessingAttributes <em>Post
 * Processing Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getNestedInvocations <em>Nested
 * Invocations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getParentInvocation <em>Parent
 * Invocation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getTestMethodName <em>Test Method
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getExpectedSize <em>Expected
 * Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#getExpectedSizeOperator <em>Expected
 * Size Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl#isExpectedReturnNull <em>Expected
 * Return Null</em>}</li>
 * </ul>
 * @generated
 */
public class IntegrationMethodTestInvocationImpl extends EObjectImpl implements IntegrationMethodTestInvocation {
	/**
	 * The default value of the '{@link #isExpectToFail() <em>Expect To Fail</em>}' attribute
	 * @see #isExpectToFail()
	 * @generated
	 * @ordered
	 */
	protected static final boolean EXPECT_TO_FAIL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isExpectToFail() <em>Expect To Fail</em>}' attribute
	 * @see #isExpectToFail()
	 * @generated
	 * @ordered
	 */
	protected boolean expectToFail = EXPECT_TO_FAIL_EDEFAULT;

	/**
	 * The default value of the '{@link #getPostProcessingStatement() <em>Post Processing Statement</em>}' attribute
	 * @see #getPostProcessingStatement()
	 * @generated
	 * @ordered
	 */
	protected static final String POST_PROCESSING_STATEMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPostProcessingStatement() <em>Post Processing Statement</em>}' attribute
	 * @see #getPostProcessingStatement()
	 * @generated
	 * @ordered
	 */
	protected String postProcessingStatement = POST_PROCESSING_STATEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getTimeout() <em>Timeout</em>}' attribute
	 * @see #getTimeout()
	 * @generated
	 * @ordered
	 */
	protected static final Integer TIMEOUT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTimeout() <em>Timeout</em>}' attribute
	 * @see #getTimeout()
	 * @generated
	 * @ordered
	 */
	protected Integer timeout = TIMEOUT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getIntegrationMethod() <em>Integration Method</em>}' reference
	 * @see #getIntegrationMethod()
	 * @generated
	 * @ordered
	 */
	protected AbstractIntegrationMethod integrationMethod;

	/**
	 * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list
	 * @see #getParameters()
	 * @generated
	 * @ordered
	 */
	protected EList<MethodInvocationParameter> parameters;

	/**
	 * The cached value of the '{@link #getReturnValues() <em>Return Values</em>}' containment reference list
	 * @see #getReturnValues()
	 * @generated
	 * @ordered
	 */
	protected EList<TestDataObject> returnValues;

	/**
	 * The cached value of the '{@link #getPostProcessingAttributes() <em>Post Processing Attributes</em>}' reference list
	 * @see #getPostProcessingAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<TestDataAttribute> postProcessingAttributes;

	/**
	 * The cached value of the '{@link #getNestedInvocations() <em>Nested Invocations</em>}' containment reference list
	 * @see #getNestedInvocations()
	 * @generated
	 * @ordered
	 */
	protected EList<IntegrationMethodTestInvocation> nestedInvocations;

	/**
	 * The default value of the '{@link #getTestMethodName() <em>Test Method Name</em>}' attribute
	 * @see #getTestMethodName()
	 * @generated
	 * @ordered
	 */
	protected static final String TEST_METHOD_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTestMethodName() <em>Test Method Name</em>}' attribute
	 * @see #getTestMethodName()
	 * @generated
	 * @ordered
	 */
	protected String testMethodName = TEST_METHOD_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getExpectedSize() <em>Expected Size</em>}' attribute
	 * @see #getExpectedSize()
	 * @generated
	 * @ordered
	 */
	protected static final Integer EXPECTED_SIZE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getExpectedSize() <em>Expected Size</em>}' attribute
	 * @see #getExpectedSize()
	 * @generated
	 * @ordered
	 */
	protected Integer expectedSize = EXPECTED_SIZE_EDEFAULT;

	/**
	 * The default value of the '{@link #getExpectedSizeOperator() <em>Expected Size Operator</em>}' attribute
	 * @see #getExpectedSizeOperator()
	 * @generated
	 * @ordered
	 */
	protected static final AssertionOperator EXPECTED_SIZE_OPERATOR_EDEFAULT = AssertionOperator.NONE;

	/**
	 * The cached value of the '{@link #getExpectedSizeOperator() <em>Expected Size Operator</em>}' attribute
	 * @see #getExpectedSizeOperator()
	 * @generated
	 * @ordered
	 */
	protected AssertionOperator expectedSizeOperator = EXPECTED_SIZE_OPERATOR_EDEFAULT;

	/**
	 * The default value of the '{@link #isExpectedReturnNull() <em>Expected Return Null</em>}' attribute
	 * @see #isExpectedReturnNull()
	 * @generated
	 * @ordered
	 */
	protected static final boolean EXPECTED_RETURN_NULL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isExpectedReturnNull() <em>Expected Return Null</em>}' attribute
	 * @see #isExpectedReturnNull()
	 * @generated
	 * @ordered
	 */
	protected boolean expectedReturnNull = EXPECTED_RETURN_NULL_EDEFAULT;

	/**
	 * @generated
	 */
	protected IntegrationMethodTestInvocationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.INTEGRATION_METHOD_TEST_INVOCATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectToFail()
	 * @generated
	 */
	@Override
	public boolean isExpectToFail() {
		return expectToFail;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setExpectToFail(boolean)
	 * @generated
	 */
	@Override
	public void setExpectToFail(boolean newExpectToFail) {
		final boolean oldExpectToFail = expectToFail;
		expectToFail = newExpectToFail;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL,
					oldExpectToFail, expectToFail));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingStatement()
	 * @generated
	 */
	@Override
	public String getPostProcessingStatement() {
		return postProcessingStatement;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setPostProcessingStatement(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPostProcessingStatement(String newPostProcessingStatement) {
		final String oldPostProcessingStatement = postProcessingStatement;
		postProcessingStatement = newPostProcessingStatement;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT, oldPostProcessingStatement,
					postProcessingStatement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTimeout()
	 * @generated
	 */
	@Override
	public Integer getTimeout() {
		return timeout;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setTimeout(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setTimeout(Integer newTimeout) {
		final Integer oldTimeout = timeout;
		timeout = newTimeout;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT,
					oldTimeout, timeout));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIntegrationMethod()
	 * @generated
	 */
	@Override
	public AbstractIntegrationMethod getIntegrationMethod() {
		if (integrationMethod != null && integrationMethod.eIsProxy()) {
			final var oldIntegrationMethod = (InternalEObject) integrationMethod;
			integrationMethod = (AbstractIntegrationMethod) eResolveProxy(oldIntegrationMethod);

			if ((integrationMethod != oldIntegrationMethod) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD, oldIntegrationMethod, integrationMethod));
		}

		return integrationMethod;
	}

	/**
	 * @return the integration method
	 * @generated
	 */
	public AbstractIntegrationMethod basicGetIntegrationMethod() {
		return integrationMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#
	 * setIntegrationMethod(net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod)
	 * @generated
	 */
	@Override
	public void setIntegrationMethod(AbstractIntegrationMethod newIntegrationMethod) {
		final AbstractIntegrationMethod oldIntegrationMethod = integrationMethod;
		integrationMethod = newIntegrationMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD,
					oldIntegrationMethod, integrationMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParameters()
	 * @generated
	 */
	@Override
	public EList<MethodInvocationParameter> getParameters() {
		if (parameters == null)
			parameters = new EObjectContainmentEList<>(MethodInvocationParameter.class, this,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS);

		return parameters;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getReturnValues()
	 * @generated
	 */
	@Override
	public EList<TestDataObject> getReturnValues() {
		if (returnValues == null)
			returnValues = new EObjectContainmentEList<>(TestDataObject.class, this,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES);

		return returnValues;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingAttributes()
	 * @generated
	 */
	@Override
	public EList<TestDataAttribute> getPostProcessingAttributes() {
		if (postProcessingAttributes == null)
			postProcessingAttributes = new EObjectResolvingEList<>(TestDataAttribute.class, this,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES);

		return postProcessingAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations()
	 * @generated
	 */
	@Override
	public EList<IntegrationMethodTestInvocation> getNestedInvocations() {
		if (nestedInvocations == null)
			nestedInvocations = new EObjectContainmentWithInverseEList<>(IntegrationMethodTestInvocation.class, this,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION);

		return nestedInvocations;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation()
	 * @generated
	 */
	@Override
	public IntegrationMethodTestInvocation getParentInvocation() {
		if (eContainerFeatureID() != TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION)
			return null;

		return (IntegrationMethodTestInvocation) eInternalContainer();
	}

	/**
	 * @param newParentInvocation
	 * @param msgs
	 * @return @generated
	 */
	public NotificationChain basicSetParentInvocation(IntegrationMethodTestInvocation newParentInvocation, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newParentInvocation,
				TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION, msgs);

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation
	 * #setParentInvocation(net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation)
	 * @generated
	 */
	@Override
	public void setParentInvocation(IntegrationMethodTestInvocation newParentInvocation) {
		if (newParentInvocation != eInternalContainer()
				|| (eContainerFeatureID() != TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION
						&& newParentInvocation != null)) {

			if (EcoreUtil.isAncestor(this, newParentInvocation))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newParentInvocation != null)
				msgs = ((InternalEObject) newParentInvocation).eInverseAdd(this,
						TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS, IntegrationMethodTestInvocation.class, msgs);

			msgs = basicSetParentInvocation(newParentInvocation, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION,
					newParentInvocation, newParentInvocation));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTestMethodName()
	 * @generated
	 */
	@Override
	public String getTestMethodName() {
		return testMethodName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setTestMethodName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTestMethodName(String newTestMethodName) {
		final String oldTestMethodName = testMethodName;
		testMethodName = newTestMethodName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME,
					oldTestMethodName, testMethodName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSize()
	 * @generated
	 */
	@Override
	public Integer getExpectedSize() {
		return expectedSize;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setExpectedSize(java.lang.Integer)
	 */
	@Override
	public void setExpectedSize(Integer newExpectedSize) {
		final Integer oldExpectedSize = expectedSize;
		expectedSize = newExpectedSize;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE,
					oldExpectedSize, expectedSize));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSizeOperator()
	 * @generated
	 */
	@Override
	public AssertionOperator getExpectedSizeOperator() {
		return expectedSizeOperator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#
	 * setExpectedSizeOperator(net.codecadenza.eclipse.model.testing.AssertionOperator)
	 * @generated
	 */
	@Override
	public void setExpectedSizeOperator(AssertionOperator newExpectedSizeOperator) {
		final AssertionOperator oldExpectedSizeOperator = expectedSizeOperator;
		expectedSizeOperator = newExpectedSizeOperator == null ? EXPECTED_SIZE_OPERATOR_EDEFAULT : newExpectedSizeOperator;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR,
							oldExpectedSizeOperator, expectedSizeOperator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectedReturnNull()
	 * @generated
	 */
	@Override
	public boolean isExpectedReturnNull() {
		return expectedReturnNull;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#setExpectedReturnNull(boolean)
	 * @generated
	 */
	@Override
	public void setExpectedReturnNull(boolean newExpectedReturnNull) {
		final boolean oldExpectedReturnNull = expectedReturnNull;
		expectedReturnNull = newExpectedReturnNull;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL, oldExpectedReturnNull, expectedReturnNull));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl# eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getNestedInvocations()).basicAdd(otherEnd, msgs);
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);
				return basicSetParentInvocation((IntegrationMethodTestInvocation) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl# eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS:
				return ((InternalEList<?>) getParameters()).basicRemove(otherEnd, msgs);
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES:
				return ((InternalEList<?>) getReturnValues()).basicRemove(otherEnd, msgs);
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				return ((InternalEList<?>) getNestedInvocations()).basicRemove(otherEnd, msgs);
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				return basicSetParentInvocation(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				return eInternalContainer().eInverseRemove(this, TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS,
						IntegrationMethodTestInvocation.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL:
				return isExpectToFail();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT:
				return getPostProcessingStatement();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT:
				return getTimeout();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD:
				if (resolve)
					return getIntegrationMethod();

				return basicGetIntegrationMethod();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS:
				return getParameters();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES:
				return getReturnValues();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES:
				return getPostProcessingAttributes();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				return getNestedInvocations();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				return getParentInvocation();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME:
				return getTestMethodName();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE:
				return getExpectedSize();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR:
				return getExpectedSizeOperator();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL:
				return isExpectedReturnNull();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL:
				setExpectToFail((Boolean) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT:
				setPostProcessingStatement((String) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT:
				setTimeout((Integer) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD:
				setIntegrationMethod((AbstractIntegrationMethod) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS:
				getParameters().clear();
				getParameters().addAll((Collection<? extends MethodInvocationParameter>) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES:
				getReturnValues().clear();
				getReturnValues().addAll((Collection<? extends TestDataObject>) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES:
				getPostProcessingAttributes().clear();
				getPostProcessingAttributes().addAll((Collection<? extends TestDataAttribute>) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				getNestedInvocations().clear();
				getNestedInvocations().addAll((Collection<? extends IntegrationMethodTestInvocation>) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				setParentInvocation((IntegrationMethodTestInvocation) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME:
				setTestMethodName((String) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE:
				setExpectedSize((Integer) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR:
				setExpectedSizeOperator((AssertionOperator) newValue);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL:
				setExpectedReturnNull((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL:
				setExpectToFail(EXPECT_TO_FAIL_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT:
				setPostProcessingStatement(POST_PROCESSING_STATEMENT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT:
				setTimeout(TIMEOUT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD:
				setIntegrationMethod((AbstractIntegrationMethod) null);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS:
				getParameters().clear();
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES:
				getReturnValues().clear();
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES:
				getPostProcessingAttributes().clear();
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				getNestedInvocations().clear();
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				setParentInvocation((IntegrationMethodTestInvocation) null);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME:
				setTestMethodName(TEST_METHOD_NAME_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE:
				setExpectedSize(EXPECTED_SIZE_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR:
				setExpectedSizeOperator(EXPECTED_SIZE_OPERATOR_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL:
				setExpectedReturnNull(EXPECTED_RETURN_NULL_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL:
				return expectToFail != EXPECT_TO_FAIL_EDEFAULT;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT:
				return postProcessingStatement != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT:
				return timeout != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD:
				return integrationMethod != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS:
				return parameters != null && !parameters.isEmpty();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES:
				return returnValues != null && !returnValues.isEmpty();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES:
				return postProcessingAttributes != null && !postProcessingAttributes.isEmpty();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS:
				return nestedInvocations != null && !nestedInvocations.isEmpty();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION:
				return getParentInvocation() != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME:
				return testMethodName != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE:
				return expectedSize != null;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR:
				return expectedSizeOperator != EXPECTED_SIZE_OPERATOR_EDEFAULT;
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL:
				return expectedReturnNull != EXPECTED_RETURN_NULL_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (expectToFail: ");
		result.append(expectToFail);
		result.append(", postProcessingStatement: ");
		result.append(postProcessingStatement);
		result.append(", timeout: ");
		result.append(timeout);
		result.append(", testMethodName: ");
		result.append(testMethodName);
		result.append(", expectedSize: ");
		result.append(expectedSize);
		result.append(", expectedSizeOperator: ");
		result.append(expectedSizeOperator);
		result.append(", expectedReturnNull: ");
		result.append(expectedReturnNull);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isReturnList()
	 * @generated not
	 */
	@Override
	public boolean isReturnList() {
		// We assume that everything else than 'NONE' represents a Collection, a List or an ArrayList!
		return getIntegrationMethod().getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isReturnVoid()
	 * @generated not
	 */
	@Override
	public boolean isReturnVoid() {
		final boolean returnVoid = getIntegrationMethod().getReturnType().isVoid();

		if (returnVoid)
			return true;

		// A Kafka integration client method without a response schema will always return null. Thus, it makes no sense to
		// evaluate the return value! A JMS integration client method that doesn't send a response also returns void!
		if (getIntegrationMethod() instanceof final KafkaIntegrationMethod kafkaMethod)
			return kafkaMethod.getResponseSchemaName() == null || kafkaMethod.getResponseSchemaName().isEmpty();
		else if (getIntegrationMethod() instanceof final JMSIntegrationMethod jmsMethod)
			return !jmsMethod.isSendResponse();

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTrackedAttributes()
	 * @generated not
	 */
	@Override
	public List<TestDataAttribute> getTrackedAttributes() {
		final List<TestDataAttribute> trackedAttributes = new ArrayList<>();

		if (isExpectToFail())
			return trackedAttributes;

		for (final TestDataObject testObject : getReturnValues())
			for (final TestDataAttribute attribute : testObject.getAttributes())
				if (attribute.getValue() == null && attribute.isTrackValue())
					trackedAttributes.add(attribute);

		for (final MethodInvocationParameter parameter : getParameters())
			for (final TestDataObject testObject : parameter.getParameterValues())
				for (final TestDataAttribute attribute : testObject.getAttributes())
					if (attribute.getValue() == null && attribute.isTrackValue())
						trackedAttributes.add(attribute);

		return trackedAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getAllAttributes()
	 * @generated not
	 */
	@Override
	public List<TestDataAttribute> getAllAttributes() {
		final List<TestDataAttribute> allAttributes = new ArrayList<>();

		for (final TestDataObject testObject : getReturnValues())
			allAttributes.addAll(getAllAttributesOfTestObject(testObject));

		for (final MethodInvocationParameter parameter : getParameters())
			for (final TestDataObject testObject : parameter.getParameterValues())
				allAttributes.addAll(getAllAttributesOfTestObject(testObject));

		return allAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#
	 * getIdsOfTrackedAttributes(net.codecadenza.eclipse.model.mapping.MappingAttribute)
	 * @generated not
	 */
	@Override
	public List<String> getIdsOfTrackedAttributes(MappingAttribute mappingAttribute) {
		final List<String> ids = new ArrayList<>();

		final List<IntegrationMethodTestInvocation> invocations = new ArrayList<>();
		invocations.add(this);
		invocations.addAll(getNestedInvocations());

		for (final IntegrationMethodTestInvocation invocation : invocations)
			for (final TestDataAttribute testDataAttribute : invocation.getTrackedAttributes())
				if (mappingAttribute == null) {
					if (testDataAttribute.getMappingAttribute() == null)
						ids.add(testDataAttribute.getId());
				}
				else if (mappingAttribute.equals(testDataAttribute.getMappingAttribute()))
					ids.add(testDataAttribute.getId());

		return ids;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIdsOfExpectedSizeFields()
	 * @generated not
	 */
	@Override
	public List<String> getIdsOfExpectedSizeFields() {
		final List<String> attributeIds = new ArrayList<>();
		boolean idFound = false;

		for (final TestDataObject testDataObject : getReturnValues())
			for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes())
				if (testDataAttribute.getExpectedSize() != null) {
					attributeIds.add(testDataAttribute.getId());
					idFound = true;
				}

		// Do not search for IDs in nested invocations if the parent invocation doesn't check the expected size!
		if (idFound)
			for (final IntegrationMethodTestInvocation nestedInvocation : getNestedInvocations())
				for (final TestDataObject testDataObject : nestedInvocation.getReturnValues())
					for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes())
						if (testDataAttribute.getExpectedSize() != null)
							attributeIds.add(testDataAttribute.getId());

		return attributeIds;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#extractAttributeNamesFromStatement()
	 * @generated not
	 */
	@Override
	public List<String> extractAttributeNamesFromStatement() {
		final List<String> attributeNames = new ArrayList<>();

		if (getPostProcessingStatement() == null || getPostProcessingStatement().isEmpty())
			return attributeNames;

		final Matcher matcher = PLACEHOLDER_PATTERN.matcher(getPostProcessingStatement());

		while (matcher.find()) {
			// Group 1 contains the Java identifier part after the colon
			attributeNames.add(matcher.group(1));
		}

		return attributeNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isFileHandlingRequired()
	 * @generated not
	 */
	@Override
	public boolean isFileHandlingRequired() {
		return isUploadFile() || isDownloadFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isUploadFile()
	 * @generated not
	 */
	@Override
	public boolean isUploadFile() {
		final BoundaryMethodTypeEnumeration methodType = getIntegrationMethod().getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.UPLOAD)
			return true;

		if (methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT) {
			final DataExchangeMethod exchangeMethod = (DataExchangeMethod) getIntegrationMethod().getBoundaryMethod()
					.getServiceMethod();

			return exchangeMethod.hasPathParameter();
		}

		return (methodType == BoundaryMethodTypeEnumeration.CREATE || methodType == BoundaryMethodTypeEnumeration.UPDATE
				|| methodType == BoundaryMethodTypeEnumeration.SAVE) && getFilePathAttribute() != null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isDownloadFile()
	 * @generated not
	 */
	@Override
	public boolean isDownloadFile() {
		final BoundaryMethodTypeEnumeration methodType = getIntegrationMethod().getBoundaryMethod().getMethodType();

		// If an asynchronous method doesn't provide a response it won't be possible to download the corresponding file!
		if (getIntegrationMethod().getReturnType().isVoid()
				|| (getIntegrationMethod() instanceof final KafkaIntegrationMethod kafkaMethod
						&& (kafkaMethod.getResponseSchemaName() == null || kafkaMethod.getResponseSchemaName().isEmpty()))
				|| (getIntegrationMethod() instanceof final JMSIntegrationMethod jmsMethod && !jmsMethod.isSendResponse()))
			return false;

		if (methodType == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT) {
			final DataExchangeMethod exchangeMethod = (DataExchangeMethod) getIntegrationMethod().getBoundaryMethod()
					.getServiceMethod();

			return exchangeMethod.returnsPath();
		}

		return methodType == BoundaryMethodTypeEnumeration.DOWNLOAD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getFilePathAttribute()
	 * @generated not
	 */
	@Override
	public TestDataAttribute getFilePathAttribute() {
		final BoundaryMethodTypeEnumeration methodType = getIntegrationMethod().getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.UPLOAD)
			return getParameters().getLast().getParameterValues().getFirst().getAttributes().getFirst();

		for (final MethodInvocationParameter parameter : getParameters()) {
			if (parameter.isRepresentsList() || parameter.getParameterValues().isEmpty())
				continue;

			final TestDataObject testDataObject = parameter.getParameterValues().getFirst();

			for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes())
				if (testDataAttribute.isMappedToFile())
					return testDataAttribute;
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getFileNameAttribute()
	 * @generated not
	 */
	@Override
	public TestDataAttribute getFileNameAttribute() {
		for (final MethodInvocationParameter parameter : getParameters()) {
			if (parameter.isRepresentsList() || parameter.getParameterValues().isEmpty())
				continue;

			final TestDataObject testDataObject = parameter.getParameterValues().getFirst();

			for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes()) {
				final MappingAttribute mappingAttribute = testDataAttribute.getMappingAttribute();

				if (mappingAttribute != null && mappingAttribute.getDomainAttribute() != null
						&& mappingAttribute.getDomainAttribute().getTag() == AttributeTagEnumeration.DOCUMENT_NAME)
					return testDataAttribute;
			}
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isWaitForResponseParameterRequired()
	 * @generated not
	 */
	@Override
	public boolean isWaitForResponseParameterRequired() {
		if (getIntegrationMethod() instanceof final KafkaIntegrationMethod kafkaIntegrationMethod)
			return kafkaIntegrationMethod.isSendResponse();

		if (getIntegrationMethod() instanceof final JMSIntegrationMethod jmsIntegrationMethod)
			return jmsIntegrationMethod.isSendResponse();

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#canReturnNull()
	 * @generated not
	 */
	@Override
	public boolean canReturnNull() {
		final BoundaryMethodTypeEnumeration methodType = getIntegrationMethod().getBoundaryMethod().getMethodType();

		return methodType == BoundaryMethodTypeEnumeration.FIND_EXISTING
				|| methodType == BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY;
	}

	/**
	 * Recursive method to collect all {@link TestDataAttribute}s of the given {@link TestDataObject}
	 * @param testDataObject
	 * @return a list with all attributes
	 * @generated not
	 */
	private List<TestDataAttribute> getAllAttributesOfTestObject(TestDataObject testDataObject) {
		final List<TestDataAttribute> allAttributes = new ArrayList<>();

		for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes()) {
			allAttributes.add(testDataAttribute);

			for (final TestDataObject referencedObject : testDataAttribute.getReferencedObjects())
				allAttributes.addAll(getAllAttributesOfTestObject(referencedObject));
		}

		return allAttributes;
	}

}

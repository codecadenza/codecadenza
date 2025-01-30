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
package net.codecadenza.runtime.crypto;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * <p>
 * Utility class for encrypting a text by using MD5 or SHA-256
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class HashGenerator {
	private static final String MD5 = "MD5";
	private static final String SHA256 = "SHA-256";

	/**
	 * Prevent instantiation
	 */
	private HashGenerator() {

	}

	/**
	 * Convert an array of bytes into hex format
	 * @param data
	 * @return the array of bytes in hex format
	 */
	private static String convertToHex(byte[] data) {
		final var b = new StringBuilder();

		for (final byte aByte : data) {
			int halfbyte = (aByte >>> 4) & 0x0F;
			int two_halfs = 0;

			do {
				if ((0 <= halfbyte) && (halfbyte <= 9))
					b.append((char) ('0' + halfbyte));
				else
					b.append((char) ('a' + (halfbyte - 10)));

				halfbyte = aByte & 0x0F;
			}
			while (two_halfs++ < 1);
		}

		return b.toString();
	}

	/**
	 * Perform encryption of the given text by using MD5
	 * @param text
	 * @return the encrypted text in hex format
	 * @throws NoSuchAlgorithmException if the selected algorithm doesn't exist
	 */
	public static String encryptMD5(String text) throws NoSuchAlgorithmException {
		return encrypt(text, MD5, StandardCharsets.UTF_8);
	}

	/**
	 * Perform encryption of the given text by using SHA-256
	 * @param text
	 * @return the encrypted text in hex format
	 * @throws NoSuchAlgorithmException if the selected algorithm doesn't exist
	 */
	public static String encryptSHA256(String text) throws NoSuchAlgorithmException {
		return encrypt(text, SHA256, StandardCharsets.UTF_8);
	}

	/**
	 * Perform encryption of the given text
	 * @param text
	 * @param digester
	 * @param charset
	 * @return the encrypted text in hex format
	 * @throws NoSuchAlgorithmException if the selected algorithm doesn't exist
	 */
	public static String encrypt(String text, String digester, Charset charset) throws NoSuchAlgorithmException {
		final MessageDigest md = MessageDigest.getInstance(digester);
		md.update(text.getBytes(charset), 0, text.length());

		return convertToHex(md.digest());
	}

}
